//===-- linker.cpp --------------------------------------------------------===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license. See the LICENSE
// file for details.
//
//===----------------------------------------------------------------------===//

#include "driver/linker.h"
#include "mars.h"
#include "module.h"
#include "root.h"
#include "driver/archiver.h"
#include "driver/cl_options.h"
#include "driver/exe_path.h"
#include "driver/tool.h"
#include "gen/irstate.h"
#include "gen/llvm.h"
#include "gen/logger.h"
#include "gen/optimizer.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/ProfileData/InstrProf.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include <algorithm>

//////////////////////////////////////////////////////////////////////////////

static llvm::cl::opt<bool> staticFlag(
    "static",
    llvm::cl::desc(
        "Create a statically linked binary, including all system dependencies"),
    llvm::cl::ZeroOrMore);

static llvm::cl::opt<std::string> mscrtlib(
    "mscrtlib",
    llvm::cl::desc(
        "MS C runtime library to link against (libcmt[d] / msvcrt[d])"),
    llvm::cl::value_desc("name"), llvm::cl::ZeroOrMore);

static llvm::cl::opt<std::string>
    ltoLibrary("flto-binary",
               llvm::cl::desc("Set the linker LTO plugin library file (e.g. "
                              "LLVMgold.so (Unixes) or libLTO.dylib (Darwin))"),
               llvm::cl::value_desc("file"), llvm::cl::ZeroOrMore);

static llvm::cl::opt<std::string> ar("ar", llvm::cl::desc("Archiver"),
                                     llvm::cl::Hidden, llvm::cl::ZeroOrMore);

//////////////////////////////////////////////////////////////////////////////

static void CreateDirectoryOnDisk(llvm::StringRef fileName) {
  auto dir = llvm::sys::path::parent_path(fileName);
  if (!dir.empty() && !llvm::sys::fs::exists(dir)) {
    if (auto ec = llvm::sys::fs::create_directories(dir)) {
      error(Loc(), "failed to create path to file: %s\n%s", dir.data(),
            ec.message().c_str());
      fatal();
    }
  }
}

//////////////////////////////////////////////////////////////////////////////

static std::string getOutputName(bool const sharedLib) {
  const auto &triple = *global.params.targetTriple;

  const char *extension = nullptr;
  if (sharedLib) {
    extension = global.dll_ext;
  } else if (triple.isOSWindows()) {
    extension = "exe";
  }

  if (global.params.exefile) {
    // DMD adds the default extension if there is none
    return opts::invokedByLDMD && extension
               ? FileName::defaultExt(global.params.exefile, extension)
               : global.params.exefile;
  }

  // Infer output name from first object file.
  std::string result = global.params.objfiles->dim
                           ? FileName::removeExt((*global.params.objfiles)[0])
                           : "a.out";

  if (sharedLib && !triple.isWindowsMSVCEnvironment())
    result = "lib" + result;

  if (global.params.run) {
    // If `-run` is passed, the executable is temporary and is removed
    // after execution. Make sure the name does not collide with other files
    // from other processes by creating a unique filename.
    llvm::SmallString<128> tempFilename;
    auto EC = llvm::sys::fs::createTemporaryFile(FileName::name(result.c_str()),
                                                 extension ? extension : "",
                                                 tempFilename);
    if (!EC)
      result = tempFilename.str();
  } else if (extension) {
    result += '.';
    result += extension;
  }

  return result;
}

//////////////////////////////////////////////////////////////////////////////
// LTO functionality

namespace {

void addLinkerFlag(std::vector<std::string> &args, const llvm::Twine &flag) {
  args.push_back("-Xlinker");
  args.push_back(flag.str());
}

std::string getLTOGoldPluginPath() {
  if (!ltoLibrary.empty()) {
    if (llvm::sys::fs::exists(ltoLibrary))
      return ltoLibrary;

    error(Loc(), "-flto-binary: file '%s' not found", ltoLibrary.c_str());
    fatal();
  } else {
    std::string searchPaths[] = {
      // The plugin packaged with LDC has a "-ldc" suffix.
      exe_path::prependLibDir("LLVMgold-ldc.so"),
      // Perhaps the user copied the plugin to LDC's lib dir.
      exe_path::prependLibDir("LLVMgold.so"),
#if __LP64__
      "/usr/local/lib64/LLVMgold.so",
#endif
      "/usr/local/lib/LLVMgold.so",
#if __LP64__
      "/usr/lib64/LLVMgold.so",
#endif
      "/usr/lib/LLVMgold.so",
      "/usr/lib/bfd-plugins/LLVMgold.so",
    };

    // Try all searchPaths and early return upon the first path found.
    for (const auto &p : searchPaths) {
      if (llvm::sys::fs::exists(p))
        return p;
    }

    error(Loc(), "The LLVMgold.so plugin (needed for LTO) was not found. You "
                 "can specify its path with -flto-binary=<file>.");
    fatal();
  }
}

void addLTOGoldPluginFlags(std::vector<std::string> &args) {
  addLinkerFlag(args, "-plugin");
  addLinkerFlag(args, getLTOGoldPluginPath());

  if (opts::isUsingThinLTO())
    addLinkerFlag(args, "-plugin-opt=thinlto");

  if (!opts::mCPU.empty())
    addLinkerFlag(args, llvm::Twine("-plugin-opt=mcpu=") + opts::mCPU);

  // Use the O-level passed to LDC as the O-level for LTO, but restrict it to
  // the [0, 3] range that can be passed to the linker plugin.
  static char optChars[15] = "-plugin-opt=O0";
  optChars[13] = '0' + std::min<char>(optLevel(), 3);
  addLinkerFlag(args, optChars);

#if LDC_LLVM_VER >= 400
  const llvm::TargetOptions &TO = gTargetMachine->Options;
  if (TO.FunctionSections)
    addLinkerFlag(args, "-plugin-opt=-function-sections");
  if (TO.DataSections)
    addLinkerFlag(args, "-plugin-opt=-data-sections");
#endif
}

// Returns an empty string when libLTO.dylib was not specified nor found.
std::string getLTOdylibPath() {
  if (!ltoLibrary.empty()) {
    if (llvm::sys::fs::exists(ltoLibrary))
      return ltoLibrary;

    error(Loc(), "-flto-binary: '%s' not found", ltoLibrary.c_str());
    fatal();
  } else {
    // The plugin packaged with LDC has a "-ldc" suffix.
    std::string searchPath = exe_path::prependLibDir("libLTO-ldc.dylib");
    if (llvm::sys::fs::exists(searchPath))
      return searchPath;

    return "";
  }
}

void addDarwinLTOFlags(std::vector<std::string> &args) {
  std::string dylibPath = getLTOdylibPath();
  if (!dylibPath.empty()) {
      args.push_back("-lto_library");
      args.push_back(std::move(dylibPath));
  }
}

/// Adds the required linker flags for LTO builds to args.
void addLTOLinkFlags(std::vector<std::string> &args) {
#if LDC_LLVM_VER >= 309
  if (global.params.targetTriple->isOSLinux() ||
      global.params.targetTriple->isOSFreeBSD() ||
      global.params.targetTriple->isOSNetBSD() ||
      global.params.targetTriple->isOSOpenBSD() ||
      global.params.targetTriple->isOSDragonFly()) {
    // Assume that ld.gold or ld.bfd is used with plugin support.
    addLTOGoldPluginFlags(args);
  } else if (global.params.targetTriple->isOSDarwin()) {
    addDarwinLTOFlags(args);
  }
#endif
}
} // anonymous namespace

//////////////////////////////////////////////////////////////////////////////

namespace {

#if LDC_LLVM_VER >= 306
/// Insert an LLVM bitcode file into the module
void insertBitcodeIntoModule(const char *bcFile, llvm::Module &M,
                             llvm::LLVMContext &Context) {
  Logger::println("*** Linking-in bitcode file %s ***", bcFile);

  llvm::SMDiagnostic Err;
  std::unique_ptr<llvm::Module> loadedModule(
      getLazyIRFileModule(bcFile, Err, Context));
  if (!loadedModule) {
    error(Loc(), "Error when loading LLVM bitcode file: %s", bcFile);
    fatal();
  }
#if LDC_LLVM_VER >= 308
  llvm::Linker(M).linkInModule(std::move(loadedModule));
#else
  llvm::Linker(&M).linkInModule(loadedModule.release());
#endif
}
#endif
}

/// Insert LLVM bitcode files into the module
void insertBitcodeFiles(llvm::Module &M, llvm::LLVMContext &Ctx,
                        Array<const char *> &bitcodeFiles) {
#if LDC_LLVM_VER >= 306
  for (const char *fname : bitcodeFiles) {
    insertBitcodeIntoModule(fname, M, Ctx);
  }
#else
  if (!bitcodeFiles.empty()) {
    error(Loc(),
          "Passing LLVM bitcode files to LDC is not supported for LLVM < 3.6");
    fatal();
  }
#endif
}

void addRtCompileLibs(std::vector<std::string>& args, bool isMsvc) {
  if (isMsvc) {
    args.push_back("ldc-rtcompile-rt.lib");
    args.push_back("rtcompile.lib");
  }
  else {
    args.push_back("-lldc-rtcompile-rt");
    args.push_back("-lrtcompile");
  }
}
//////////////////////////////////////////////////////////////////////////////

static void appendObjectFiles(std::vector<std::string> &args) {
  for (unsigned i = 0; i < global.params.objfiles->dim; i++)
    args.push_back((*global.params.objfiles)[i]);

  if (global.params.targetTriple->isWindowsMSVCEnvironment()) {
    if (global.params.resfile)
      args.push_back(global.params.resfile);
    if (global.params.deffile)
      args.push_back(std::string("/DEF:") + global.params.deffile);
  }
}

//////////////////////////////////////////////////////////////////////////////

static std::string gExePath;

static int linkObjToBinaryGcc(bool sharedLib) {
  Logger::println("*** Linking executable ***");

  // find gcc for linking
  const std::string tool = getGcc();

  // build arguments
  std::vector<std::string> args;

  appendObjectFiles(args);

  // Link with profile-rt library when generating an instrumented binary.
  // profile-rt uses Phobos (MD5 hashing) and therefore must be passed on the
  // commandline before Phobos.
  if (global.params.genInstrProf) {
#if LDC_LLVM_VER >= 308
    if (global.params.targetTriple->isOSLinux()) {
      // For Linux, explicitly define __llvm_profile_runtime as undefined
      // symbol, so that the initialization part of profile-rt is linked in.
      args.push_back(
          ("-Wl,-u," + llvm::getInstrProfRuntimeHookVarName()).str());
    }
#endif
    args.push_back("-lldc-profile-rt");
  }

  if (global.params.enableRuntimeCompile) {
    addRtCompileLibs(args, false);
  }

  // user libs
  for (unsigned i = 0; i < global.params.libfiles->dim; i++)
    args.push_back((*global.params.libfiles)[i]);

  // output filename
  std::string output = getOutputName(sharedLib);

  if (sharedLib) {
    args.push_back("-shared");
  }

  if (staticFlag) {
    args.push_back("-static");
  }

  args.push_back("-o");
  args.push_back(output);

  // set the global gExePath
  gExePath = output;
  // assert(gExePath.isValid());

  // create path to exe
  CreateDirectoryOnDisk(gExePath);

  // Pass sanitizer arguments to linker. Requires clang.
  if (opts::sanitize == opts::AddressSanitizer) {
    args.push_back("-fsanitize=address");
  }

  if (opts::sanitize == opts::MemorySanitizer) {
    args.push_back("-fsanitize=memory");
  }

  if (opts::sanitize == opts::ThreadSanitizer) {
    args.push_back("-fsanitize=thread");
  }

  // Add LTO link flags before adding the user link switches, such that the user
  // can pass additional options to the LTO plugin.
  if (opts::isUsingLTO())
    addLTOLinkFlags(args);

  // additional linker switches
  for (unsigned i = 0; i < global.params.linkswitches->dim; i++) {
    const char *p = (*global.params.linkswitches)[i];
    // Don't push -l and -L switches using -Xlinker, but pass them indirectly
    // via GCC. This makes sure user-defined paths take precedence over
    // GCC's builtin LIBRARY_PATHs.
    // Options starting with `-Wl,`, -shared or -static are not handled by
    // the linker and must be passed to the driver.
    auto str = llvm::StringRef(p);
    if (!(str.startswith("-l") || str.startswith("-L") ||
          str.startswith("-Wl,") ||
          str.startswith("-shared") || str.startswith("-static"))) {
      args.push_back("-Xlinker");
    }
    args.push_back(p);
  }

  // default libs
  bool addSoname = false;
  switch (global.params.targetTriple->getOS()) {
  case llvm::Triple::Linux:
    addSoname = true;
    // Make sure we don't do --gc-sections when generating a profile-
    // instrumented binary. The runtime relies on magic sections, which
    // would be stripped by gc-section on older version of ld, see bug:
    // https://sourceware.org/bugzilla/show_bug.cgi?id=19161
    if (!opts::disableLinkerStripDead && !global.params.genInstrProf) {
      args.push_back("-Wl,--gc-sections");
    }
    if (global.params.targetTriple->getEnvironment() == llvm::Triple::Android) {
      args.push_back("-ldl");
      args.push_back("-lm");
      break;
    }
    args.push_back("-lrt");
  // fallthrough
  case llvm::Triple::Darwin:
  case llvm::Triple::MacOSX:
    args.push_back("-ldl");
  // fallthrough
  case llvm::Triple::FreeBSD:
  case llvm::Triple::NetBSD:
  case llvm::Triple::OpenBSD:
  case llvm::Triple::DragonFly:
    addSoname = true;
    args.push_back("-lpthread");
    args.push_back("-lm");
    break;

  case llvm::Triple::Solaris:
    args.push_back("-lm");
    args.push_back("-lumem");
    args.push_back("-lsocket");
    args.push_back("-lnsl");
    break;

  default:
    // OS not yet handled, will probably lead to linker errors.
    // FIXME: Win32.
    break;
  }

  if (global.params.targetTriple->isWindowsGNUEnvironment()) {
    // This is really more of a kludge, as linking in the Winsock functions
    // should be handled by the pragma(lib, ...) in std.socket, but it
    // makes LDC behave as expected for now.
    args.push_back("-lws2_32");
  }

  // Only specify -m32/-m64 for architectures where the two variants actually
  // exist (as e.g. the GCC ARM toolchain doesn't recognize the switches).
  // MIPS does not have -m32/-m64 but requires -mabi=.
  if (global.params.targetTriple->get64BitArchVariant().getArch() !=
          llvm::Triple::UnknownArch &&
      global.params.targetTriple->get32BitArchVariant().getArch() !=
          llvm::Triple::UnknownArch) {
    if (global.params.targetTriple->get64BitArchVariant().getArch() ==
            llvm::Triple::mips64 ||
        global.params.targetTriple->get64BitArchVariant().getArch() ==
            llvm::Triple::mips64el) {
      switch (getMipsABI()) {
      case MipsABI::EABI:
        args.push_back("-mabi=eabi");
        break;
      case MipsABI::O32:
        args.push_back("-mabi=32");
        break;
      case MipsABI::N32:
        args.push_back("-mabi=n32");
        break;
      case MipsABI::N64:
        args.push_back("-mabi=64");
        break;
      case MipsABI::Unknown:
        break;
      }
    } else {
      switch (global.params.targetTriple->getArch()) {
      case llvm::Triple::arm:
      case llvm::Triple::armeb:
      case llvm::Triple::aarch64:
      case llvm::Triple::aarch64_be:
#if LDC_LLVM_VER == 305
      case llvm::Triple::arm64:
      case llvm::Triple::arm64_be:
#endif
        break;
      default:
        if (global.params.is64bit) {
          args.push_back("-m64");
        } else {
          args.push_back("-m32");
        }
      }
    }
  }

  if (global.params.dll && addSoname) {
    std::string soname = opts::soname;
    if (!soname.empty()) {
      args.push_back("-Wl,-soname," + soname);
    }
  }

  Logger::println("Linking with: ");
  Stream logstr = Logger::cout();
  for (const auto &arg : args) {
    if (!arg.empty()) {
      logstr << "'" << arg << "'"
             << " ";
    }
  }
  logstr << "\n"; // FIXME where's flush ?

  // try to call linker
  return executeToolAndWait(tool, args, global.params.verbose);
}

//////////////////////////////////////////////////////////////////////////////

static void addMscrtLibs(std::vector<std::string> &args) {
  llvm::StringRef mscrtlibName = mscrtlib;
  if (mscrtlibName.empty()) {
    // default to static release variant
    mscrtlibName =
        staticFlag || staticFlag.getNumOccurrences() == 0 ? "libcmt" : "msvcrt";
  }

  args.push_back(("/DEFAULTLIB:" + mscrtlibName).str());

  const bool isStatic = mscrtlibName.startswith_lower("libcmt");
  const bool isDebug =
      mscrtlibName.endswith_lower("d") || mscrtlibName.endswith_lower("d.lib");

  const llvm::StringRef prefix = isStatic ? "lib" : "";
  const llvm::StringRef suffix = isDebug ? "d" : "";

  args.push_back(("/DEFAULTLIB:" + prefix + "vcruntime" + suffix).str());
}

static int linkObjToBinaryMSVC(bool sharedLib) {
  Logger::println("*** Linking executable ***");

#ifdef _WIN32
  windows::setupMsvcEnvironment();
#endif

  const std::string tool = "link.exe";

  // build arguments
  std::vector<std::string> args;

  args.push_back("/NOLOGO");

  // specify that the image will contain a table of safe exception handlers
  // and can handle addresses >2GB (32bit only)
  if (!global.params.is64bit) {
    args.push_back("/SAFESEH");
    args.push_back("/LARGEADDRESSAWARE");
  }

  // output debug information
  if (global.params.symdebug) {
    args.push_back("/DEBUG");
  }

  // enable Link-time Code Generation (aka. whole program optimization)
  if (global.params.optimize) {
    args.push_back("/LTCG");
  }

  // remove dead code and fold identical COMDATs
  if (opts::disableLinkerStripDead) {
    args.push_back("/OPT:NOREF");
  } else {
    args.push_back("/OPT:REF");
    args.push_back("/OPT:ICF");
  }

  // add C runtime libs
  addMscrtLibs(args);

  // specify creation of DLL
  if (sharedLib) {
    args.push_back("/DLL");
  }

  // output filename
  std::string output = getOutputName(sharedLib);

  args.push_back("/OUT:" + output);

  appendObjectFiles(args);

  // Link with profile-rt library when generating an instrumented binary
  // profile-rt depends on Phobos (MD5 hashing).
  if (global.params.genInstrProf) {
    args.push_back("ldc-profile-rt.lib");
    // profile-rt depends on ws2_32 for symbol `gethostname`
    args.push_back("ws2_32.lib");
  }

  if (global.params.enableRuntimeCompile) {
    addRtCompileLibs(args, true);
  }

  // user libs
  for (unsigned i = 0; i < global.params.libfiles->dim; i++)
    args.push_back((*global.params.libfiles)[i]);

  // set the global gExePath
  gExePath = output;
  // assert(gExePath.isValid());

  // create path to exe
  CreateDirectoryOnDisk(gExePath);

  // additional linker switches
  for (unsigned i = 0; i < global.params.linkswitches->dim; i++) {
    std::string str = global.params.linkswitches->data[i];
    if (str.length() > 2) {
      // rewrite common -L and -l switches
      if (str[0] == '-' && str[1] == 'L') {
        str = "/LIBPATH:" + str.substr(2);
      } else if (str[0] == '-' && str[1] == 'l') {
        str = str.substr(2) + ".lib";
      }
    }
    args.push_back(str);
  }

  // default libs
  // TODO check which libaries are necessary
  args.push_back("kernel32.lib");
  args.push_back("user32.lib");
  args.push_back("gdi32.lib");
  args.push_back("winspool.lib");
  args.push_back("shell32.lib"); // required for dmain2.d
  args.push_back("ole32.lib");
  args.push_back("oleaut32.lib");
  args.push_back("uuid.lib");
  args.push_back("comdlg32.lib");
  args.push_back("advapi32.lib");

  Logger::println("Linking with: ");
  Stream logstr = Logger::cout();
  for (const auto &arg : args) {
    if (!arg.empty()) {
      logstr << "'" << arg << "'"
             << " ";
    }
  }
  logstr << "\n"; // FIXME where's flush ?

  // try to call linker
  return executeToolAndWait(tool, args, global.params.verbose);
}

//////////////////////////////////////////////////////////////////////////////

int linkObjToBinary() {
  if (global.params.targetTriple->isWindowsMSVCEnvironment()) {
    return linkObjToBinaryMSVC(global.params.dll);
  }

  return linkObjToBinaryGcc(global.params.dll);
}

//////////////////////////////////////////////////////////////////////////////

int createStaticLibrary() {
  Logger::println("*** Creating static library ***");

  const bool isTargetMSVC =
      global.params.targetTriple->isWindowsMSVCEnvironment();

#if LDC_LLVM_VER >= 309
  const bool useInternalArchiver = ar.empty();
#else
  const bool useInternalArchiver = false;
#endif

  // find archiver
  std::string tool;
  if (useInternalArchiver) {
    tool = isTargetMSVC ? "llvm-lib.exe" : "llvm-ar";
  } else {
#ifdef _WIN32
    if (isTargetMSVC)
      windows::setupMsvcEnvironment();
#endif

    tool = getProgram(isTargetMSVC ? "lib.exe" : "ar", &ar);
  }

  // build arguments
  std::vector<std::string> args;

  // ask ar to create a new library
  if (!isTargetMSVC) {
    args.push_back("rcs");
  }

  // ask lib to be quiet
  if (isTargetMSVC) {
    args.push_back("/NOLOGO");
  }

  // enable Link-time Code Generation (aka. whole program optimization)
  if (isTargetMSVC && global.params.optimize) {
    args.push_back("/LTCG");
  }

  // output filename
  std::string libName;
  if (global.params.libname) { // explicit
    // DMD adds the default extension if there is none
    libName = opts::invokedByLDMD
                  ? FileName::defaultExt(global.params.libname, global.lib_ext)
                  : global.params.libname;
  } else { // infer from first object file
    libName = global.params.objfiles->dim
                  ? FileName::removeExt((*global.params.objfiles)[0])
                  : "a.out";
    libName += '.';
    libName += global.lib_ext;
  }

  // DMD creates static libraries in the objects directory (unless using an
  // absolute output path via `-of`).
  if (opts::invokedByLDMD && global.params.objdir &&
      !FileName::absolute(libName.c_str())) {
    libName = FileName::combine(global.params.objdir, libName.c_str());
  }

  if (isTargetMSVC) {
    args.push_back("/OUT:" + libName);
  } else {
    args.push_back(libName);
  }

  appendObjectFiles(args);

  // create path to the library
  CreateDirectoryOnDisk(libName);

#if LDC_LLVM_VER >= 309
  if (useInternalArchiver) {
    std::vector<const char *> fullArgs;
    fullArgs.reserve(1 + args.size());
    fullArgs.push_back(tool.c_str());
    for (const auto &arg : args)
      fullArgs.push_back(arg.c_str());

    if (global.params.verbose) {
      for (auto arg : fullArgs) {
        fprintf(global.stdmsg, "%s ", arg);
      }
      fprintf(global.stdmsg, "\n");
      fflush(global.stdmsg);
    }

    const int exitCode = isTargetMSVC ? ldc::lib(fullArgs) : ldc::ar(fullArgs);
    if (exitCode)
      error(Loc(), "%s failed with status: %d", tool.c_str(), exitCode);

    return exitCode;
  }
#endif

  // try to call archiver
  return executeToolAndWait(tool, args, global.params.verbose);
}

//////////////////////////////////////////////////////////////////////////////

void deleteExeFile() {
  if (!gExePath.empty() && !llvm::sys::fs::is_directory(gExePath)) {
    llvm::sys::fs::remove(gExePath);
  }
}

//////////////////////////////////////////////////////////////////////////////

int runProgram() {
  assert(!gExePath.empty());
  // assert(gExePath.isValid());

  // Run executable
  int status =
      executeToolAndWait(gExePath, opts::runargs, global.params.verbose);
  if (status < 0) {
#if defined(_MSC_VER) || defined(__MINGW32__)
    error(Loc(), "program received signal %d", -status);
#else
    error(Loc(), "program received signal %d (%s)", -status,
          strsignal(-status));
#endif
    return -status;
  }
  return status;
}
