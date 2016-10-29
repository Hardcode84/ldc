
#include <map>

#include "llvm/Support/ManagedStatic.h"
#include "llvm/IR/Verifier.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/RuntimeDyld.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"

#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Bitcode/ReaderWriter.h"

#pragma pack(push,1)

struct RtCompileFuncList
{
  const char* name;
  void** func;
};

struct RtCompileSymList
{
  const char* name;
  void* sym;
};

struct RtComileModuleList
{
  RtComileModuleList* next;
  const char* irData;
  int irDataSize;
  RtCompileFuncList* funcList;
  int funcListSize;
  RtCompileSymList* symList;
  int symListSize;
};

#pragma pack(pop)

using SymMap = std::map<std::string, void*>;

struct llvm_init_obj {
  llvm_init_obj() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
  }
};

class MyJIT {
private:
  llvm_init_obj InitObj;
  llvm::llvm_shutdown_obj ShutdownObj;
  std::unique_ptr<llvm::TargetMachine> TM;
  const llvm::DataLayout DL;
  using ObjectLayerT = llvm::orc::ObjectLinkingLayer<>;
  ObjectLayerT ObjectLayer;
  using CompileLayerT = llvm::orc::IRCompileLayer<ObjectLayerT>;
  CompileLayerT CompileLayer;
  llvm::LLVMContext Context;

public:
  typedef CompileLayerT::ModuleSetHandleT ModuleHandle;

  MyJIT():
    TM(llvm::EngineBuilder().selectTarget(llvm::Triple(),{},{},llvm::SmallVector<std::string,0>())),
    DL(TM->createDataLayout()),
    CompileLayer(ObjectLayer, llvm::orc::SimpleCompiler(*TM))
  {

    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  }

  llvm::TargetMachine& getTargetMachine() { return *TM; }

  ModuleHandle addModules(std::vector<std::unique_ptr<llvm::Module>> &&modules,
                          const SymMap& symMap) {
    // Build our symbol resolver:
    // Lambda 1: Look back into the JIT itself to find symbols that are part of
    //           the same "logical dylib".
    // Lambda 2: Search for external symbols in the host process.
    auto Resolver = llvm::orc::createLambdaResolver(
                      [&](const std::string& name)->llvm::RuntimeDyld::SymbolInfo
    {
      if (auto Sym = CompileLayer.findSymbol(name, false))
      {
        return llvm::RuntimeDyld::SymbolInfo(Sym.getAddress(), Sym.getFlags());
      }
      return llvm::RuntimeDyld::SymbolInfo(nullptr);
    },
    [&](const std::string& name)->llvm::RuntimeDyld::SymbolInfo
    {
      auto it = symMap.find(name);
      if (symMap.end() != it) {
        return llvm::RuntimeDyld::SymbolInfo(reinterpret_cast<uint64_t>(it->second),
                                             llvm::JITSymbolFlags::Exported);
      }
      return llvm::RuntimeDyld::SymbolInfo(nullptr);
    });

    // Add the set to the JIT with the resolver we created above and a newly
    // created SectionMemoryManager.
    return CompileLayer.addModuleSet(std::move(modules),
                                     llvm::make_unique<llvm::SectionMemoryManager>(),
                                     std::move(Resolver));
  }

  llvm::orc::JITSymbol findSymbol(const std::string &name) {
    return CompileLayer.findSymbol(name, false);
  }

  llvm::LLVMContext& getContext() { return Context; }

};

static MyJIT& getJit()
{
  static MyJIT jit;
  return jit;
}

extern "C" {

#ifdef _WIN32
__declspec(dllexport)
#endif
void rtCompileProcessImplSo(const void* modlist_head) {
  MyJIT& myJit = getJit();
  auto current =
      static_cast<const RtComileModuleList*>(modlist_head);

  std::vector<std::pair<std::string, void**> > functions;
  std::vector<std::unique_ptr<llvm::Module>> ms;
  SymMap symMap;
  while (nullptr != current) {
    auto buff = llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(current->irData, current->irDataSize), "", false);
    auto mod = llvm::parseBitcodeFile(*buff, myJit.getContext());
    if (!mod) {
      //TODO
    }
    else {
      std::string err;
      llvm::raw_string_ostream errstream(err);
      if (llvm::verifyModule(**mod, &errstream)) {
        //TODO
      }
      (*mod)->setDataLayout(myJit.getTargetMachine().createDataLayout());
      ms.push_back(std::move(*mod));

      for (int i = 0; i < current->funcListSize; ++i) {
        const auto& fun = current->funcList[i];
        functions.push_back(std::make_pair(fun.name, fun.func));
      }

      for (int i = 0; i < current->symListSize; ++i) {
        const auto& sym = current->symList[i];
        symMap.insert(std::make_pair(sym.name, sym.sym));
      }
    }
    current = current->next;
  }
  myJit.addModules(std::move(ms), symMap);
  for (auto&& fun: functions) {
    auto symbol = myJit.findSymbol(fun.first);
    const auto addr = symbol.getAddress();
    if (0 == addr) {
      //TODO
    }
    else {
      *fun.second = reinterpret_cast<void*>(addr);
    }
  }
}

}
