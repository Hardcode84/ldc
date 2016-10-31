#include "optimizer.h"

#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

namespace {

void addOptimizationPasses(llvm::legacy::PassManagerBase &mpm,
                           llvm::legacy::FunctionPassManager &fpm,
                           unsigned optLevel, unsigned sizeLevel) {
//  if (!noVerify) {
//    fpm.add(createVerifierPass());
//  }

  llvm::PassManagerBuilder builder;
  builder.OptLevel = optLevel;
  builder.SizeLevel = sizeLevel;

  if (/*willInline()*/true) {
    unsigned threshold = 225;
    if (sizeLevel == 1) { // -Os
      threshold = 75;
    } else if (sizeLevel == 2) { // -Oz
      threshold = 25;
    }
    if (optLevel > 2) {
      threshold = 275;
    }
    builder.Inliner = llvm::createFunctionInliningPass(threshold);
  } else {
    builder.Inliner = llvm::createAlwaysInlinerPass();
  }
//  builder.DisableUnitAtATime = !unitAtATime;
  builder.DisableUnrollLoops = optLevel == 0;

//  builder.DisableUnrollLoops = (disableLoopUnrolling.getNumOccurrences() > 0)
//                                   ? disableLoopUnrolling
//                                   : optLevel == 0;

  // This is final, unless there is a #pragma vectorize enable
  if (/*disableLoopVectorization*/false) {
    builder.LoopVectorize = false;
    // If option wasn't forced via cmd line (-vectorize-loops, -loop-vectorize)
  } else if (!builder.LoopVectorize) {
    builder.LoopVectorize = optLevel > 1 && sizeLevel < 2;
  }

  // When #pragma vectorize is on for SLP, do the same as above
  builder.SLPVectorize =
      /*disableSLPVectorization*/false ? false : optLevel > 1 && sizeLevel < 2;

//  if (opts::sanitize == opts::AddressSanitizer) {
//    builder.addExtension(PassManagerBuilder::EP_OptimizerLast,
//                         addAddressSanitizerPasses);
//    builder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
//                         addAddressSanitizerPasses);
//  }

//  if (opts::sanitize == opts::MemorySanitizer) {
//    builder.addExtension(PassManagerBuilder::EP_OptimizerLast,
//                         addMemorySanitizerPass);
//    builder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
//                         addMemorySanitizerPass);
//  }

//  if (opts::sanitize == opts::ThreadSanitizer) {
//    builder.addExtension(PassManagerBuilder::EP_OptimizerLast,
//                         addThreadSanitizerPass);
//    builder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
//                         addThreadSanitizerPass);
//  }

//  if (!disableLangSpecificPasses) {
//    if (!disableSimplifyDruntimeCalls) {
//      builder.addExtension(PassManagerBuilder::EP_LoopOptimizerEnd,
//                           addSimplifyDRuntimeCallsPass);
//    }

//    if (!disableGCToStack) {
//      builder.addExtension(PassManagerBuilder::EP_LoopOptimizerEnd,
//                           addGarbageCollect2StackPass);
//    }
//  }

  // EP_OptimizerLast does not exist in LLVM 3.0, add it manually below.
//  builder.addExtension(llvm::PassManagerBuilder::EP_OptimizerLast,
//                       addStripExternalsPass);

//  addInstrProfilingPass(mpm);

  builder.populateFunctionPassManager(fpm);
  builder.populateModulePassManager(mpm);
}

void setupPasses(llvm::TargetMachine &targetMachine,
                 const OptimizerSettings& settings,
                 llvm::legacy::PassManager &mpm,
                 llvm::legacy::FunctionPassManager &fpm) {
  mpm.add(new llvm::TargetLibraryInfoWrapperPass(
            targetMachine.getTargetTriple()));
  mpm.add(llvm::createTargetTransformInfoWrapperPass(
            targetMachine.getTargetIRAnalysis()));
  fpm.add(llvm::createTargetTransformInfoWrapperPass(
            targetMachine.getTargetIRAnalysis()));

  if (/*stripDebug*/true) {
    mpm.add(llvm::createStripSymbolsPass(true));
  }

  addOptimizationPasses(mpm, fpm, settings.optLevel, settings.sizeLeve);
}

} // anon namespace

void optimizeModule(llvm::TargetMachine &targetMachine, const OptimizerSettings &settings, llvm::Module &module)
{
  llvm::legacy::PassManager mpm;
  llvm::legacy::FunctionPassManager fpm(&module);
  setupPasses(targetMachine, settings, mpm, fpm);

  // Run per-function passes.
  fpm.doInitialization();
  for (auto &F : module) {
    fpm.run(F);
  }
  fpm.doFinalization();

  // Run per-module passes.
  mpm.run(module);

//  llvm::verifyModule(M); TODO
}
