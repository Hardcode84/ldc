#include "runtimecompile.h"

#include "globals.h"

#include "gen/irstate.h"
#include "ir/irfunction.h"
#include "gen/llvm.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/Bitcode/ReaderWriter.h"

namespace {

const char* RuntimeCompileModulesHeadName = "runtimecompile_modules_head";

template<typename F>
void enumOperands(const llvm::User& usr, F&& handler) {
  for (auto&& op: usr.operands()) {
    llvm::Value* val = op.get();
    if (auto opusr = llvm::dyn_cast<llvm::User>(val)) {
      if (auto gv = llvm::dyn_cast<llvm::GlobalValue>(opusr)) {
        handler(gv);
      }
      enumOperands(*opusr, std::forward<F>(handler));
    }
  }
}

template<typename F>
void enumFuncSymbols(llvm::Function* fun, F&& handler) {
  assert(nullptr != fun);
  for (auto&& bb: *fun) {
    for (auto&& instr: bb) {
      enumOperands(instr, std::forward<F>(handler));
    }
  }
}

enum class GlobalValVisibility {
  Internal,
  External,
};

using GlobalValsMap = std::map<llvm::GlobalValue*,GlobalValVisibility>;

GlobalValsMap createGlobalValsFilter(IRState *irs) {
  assert(nullptr != irs);
  GlobalValsMap ret;
  std::vector<llvm::Function*> newFunctions;
  newFunctions.reserve(irs->runtimeCompiledFunctions.size());

  for (auto&& fun: irs->runtimeCompiledFunctions) {
    ret.insert(std::make_pair(fun.srcFunc, GlobalValVisibility::External));
    newFunctions.push_back(fun.srcFunc);
  }

  std::vector<llvm::Function*> functionsToAdd;
  while (!newFunctions.empty()) {
    for (auto&& fun: newFunctions) {
      enumFuncSymbols(fun, [&](llvm::GlobalValue* gv) {
        if (ret.insert(std::make_pair(gv, GlobalValVisibility::Internal)).second) {
          if (auto newFun = llvm::dyn_cast<llvm::Function>(gv)) {
            functionsToAdd.push_back(newFun);
          }
        }
      });
    }
    newFunctions.swap(functionsToAdd);
    functionsToAdd.clear();
  }
  return ret;
}

void fixupRtThunks(llvm::Module &newModule,
                   llvm::ArrayRef<IRState::RtCompiledFuncDesc> funcs) {
  std::map<std::string, std::string> thunk2func;
  for (auto&& func: funcs) {
    assert(nullptr != func.srcFunc);
    assert(nullptr != func.thunkVar);
    assert(nullptr != func.thunkFunc);
    assert(thunk2func.end() == thunk2func.find(func.thunkVar->getName()));
    thunk2func.insert(std::make_pair(func.thunkVar->getName(),
                                     func.srcFunc->getName()));
  }
  int objectsFixed = 0;
  for (auto&& obj: newModule.globals()) {
    auto it = thunk2func.find(obj.getName());
    if (thunk2func.end() != it) {
      if (obj.hasInitializer()) {
        auto func = newModule.getFunction(it->second);
        assert(nullptr != func);
        obj.setConstant(true);
        obj.setInitializer(func);
      }
      ++objectsFixed;
    }
  }
  assert(objectsFixed = thunk2func.size());
}

void hideExternalSymbols(llvm::Module &newModule, const GlobalValsMap &filter) {
  std::set<std::string> externalSymbols;
  for (auto&& val: filter) {
    if(GlobalValVisibility::External == val.second) {
      externalSymbols.emplace(val.first->getName());
    }
  }
  for (auto& obj: newModule.global_objects()) {
    if ((llvm::GlobalValue::ExternalLinkage == obj.getLinkage()) &&
        (externalSymbols.end() == externalSymbols.find(obj.getName()))) {
      obj.setLinkage(llvm::GlobalValue::InternalLinkage);
    }
  }
}

llvm::Constant *getArrayPtr(llvm::Constant *array) {
  assert(nullptr != array);
  llvm::ConstantInt *zero =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(array->getContext()), 0, false);
  llvm::Constant *idxs[] = {zero, zero};
  return llvm::ConstantExpr::getGetElementPtr(
        nullptr, array, idxs, true);
}

template<typename T>
void createStaticArray(llvm::Module& mod,
                       llvm::GlobalVariable* var,
                       llvm::GlobalVariable* varLen, //can be null
                       llvm::ArrayRef<T> arr) {
  assert(nullptr != var);
  const auto dataLen = arr.size();
  auto gvar = new llvm::GlobalVariable(
                mod,
                llvm::ArrayType::get(llvm::TypeBuilder<T, false>::get(mod.getContext()), dataLen),
                true,
                llvm::GlobalValue::InternalLinkage,
                llvm::ConstantDataArray::get(mod.getContext(), arr),
                ".str");
  var->setInitializer(getArrayPtr(gvar));
  if (nullptr != varLen) {
    varLen->setInitializer(llvm::ConstantInt::get(mod.getContext(), APInt(32, dataLen)));
  }
}

void createStaticString(llvm::Module& mod,
                        llvm::GlobalVariable* var,
                        llvm::GlobalVariable* varLen, //can be null
                        llvm::StringRef str) {
  assert(nullptr != var);
  const auto dataLen = str.size() + 1;
  auto gvar = new llvm::GlobalVariable(
                mod,
                llvm::ArrayType::get(llvm::Type::getInt8Ty(mod.getContext()), dataLen),
                true,
                llvm::GlobalValue::InternalLinkage,
                llvm::ConstantDataArray::getString(mod.getContext(), str, true),
                ".str");
  var->setInitializer(getArrayPtr(gvar));
  if (nullptr != varLen) {
    varLen->setInitializer(llvm::ConstantInt::get(mod.getContext(), APInt(32, dataLen)));
  }
}

// struct RtCompileSymList
// {
//   i8* name;
//   i8* sym;
// };

llvm::StructType *getSymListElemType(llvm::LLVMContext &context) {
  llvm::Type* elements[] = {
    llvm::IntegerType::getInt8PtrTy(context),
    llvm::IntegerType::getInt8PtrTy(context),
  };
  return llvm::StructType::create(context, elements, /*"RtCompileSymList"*/"", true);
}

// struct RtCompileFuncList
// {
//   i8* name;
//   i8* func;
// };

llvm::StructType *getFuncListElemType(llvm::LLVMContext &context) {
  llvm::Type* elements[] = {
    llvm::IntegerType::getInt8PtrTy(context),
    llvm::IntegerType::getInt8PtrTy(context),
  };
  return llvm::StructType::create(context, elements, /*"RtCompileFuncList"*/"", true);
}

// struct RtComileModuleList
// {
//   RtComileModuleList* next;
//   i8* irData;
//   i32 irDataSize;
//   RtCompileFuncList* funcList;
//   i32 funcListSize;
//   RtCompileSymList* symList;
//   i32 symListSize;
// };

llvm::StructType *getModuleListElemType(llvm::LLVMContext &context,
                                        llvm::StructType *funcListElemType,
                                        llvm::StructType *symListElemType) {
  assert(nullptr != funcListElemType);
  assert(nullptr != symListElemType);
  llvm::StructType* ret = llvm::StructType::create(context/*, "RtComileModuleList"*/); //fwddecl
  llvm::Type* elements[] = {
    llvm::PointerType::getUnqual(ret),
    llvm::IntegerType::getInt8PtrTy(context),
    llvm::IntegerType::get(context, 32),
    llvm::PointerType::getUnqual(funcListElemType),
    llvm::IntegerType::get(context, 32),
    llvm::PointerType::getUnqual(symListElemType),
    llvm::IntegerType::get(context, 32),};
  ret->setBody(elements, true);
  return ret;
}

struct Types {
  llvm::StructType *funcListElemType;
  llvm::StructType *symListElemType;
  llvm::StructType *modListElemType;

  Types(llvm::LLVMContext &context):
    funcListElemType(getFuncListElemType(context)),
    symListElemType(getSymListElemType(context)),
    modListElemType(getModuleListElemType(context,
                                          funcListElemType,
                                          symListElemType)) {}
};

std::pair<llvm::Constant*, llvm::Constant*> generateFuncList(
    IRState *irs,
    const Types &types) {
  assert(nullptr != irs);
  std::vector<llvm::Constant*> elements;
  for (auto&& func: irs->runtimeCompiledFunctions) {
    assert(nullptr != func.srcFunc);
    assert(nullptr != func.thunkVar);
    assert(nullptr != func.thunkFunc);
    auto name = func.srcFunc->getName();
    auto nameVar = new llvm::GlobalVariable(
                  irs->module,
                  llvm::ArrayType::get(llvm::Type::getInt8Ty(irs->context()), name.size() + 1),
                  true,
                  llvm::GlobalValue::PrivateLinkage,
                  llvm::ConstantDataArray::getString(irs->context(), name, true),
                  ".str");
    auto thunkPtr = llvm::ConstantExpr::getBitCast(
                      func.thunkVar,
                      llvm::IntegerType::getInt8PtrTy(irs->context()));
    llvm::Constant* fields[] = {
      llvm::ConstantExpr::getBitCast(
        nameVar,
        llvm::Type::getInt8PtrTy(irs->context())),
      thunkPtr,
    };
    elements.push_back(llvm::ConstantStruct::get(types.funcListElemType, fields));
  }
  auto arrayType = llvm::ArrayType::get(types.funcListElemType, elements.size());
  auto arrVar = new llvm::GlobalVariable(
                  irs->module,
                  arrayType,
                  true,
                  llvm::GlobalValue::PrivateLinkage,
                  llvm::ConstantArray::get(arrayType, elements),
                  ".str");
  return std::make_pair(
        getArrayPtr(arrVar),
        llvm::ConstantInt::get(irs->context(),APInt(32, elements.size())));
}

std::pair<llvm::Constant*, llvm::Constant*> generateSymList(
    IRState *irs,
    const Types &types,
    const GlobalValsMap& globalVals) {
  assert(nullptr != irs);
  std::vector<llvm::Constant*> elements;
  for (auto&& it: globalVals) {
    auto val = it.first;
    if (!val->isDeclaration() ) continue;
    if (auto fun = llvm::dyn_cast<llvm::Function>(val)) {
      if (fun->isIntrinsic()) continue;
    }
    auto name = val->getName();
    auto nameVar = new llvm::GlobalVariable(
                     irs->module,
                     llvm::ArrayType::get(llvm::Type::getInt8Ty(irs->context()), name.size() + 1),
                     true,
                     llvm::GlobalValue::PrivateLinkage,
                     llvm::ConstantDataArray::getString(irs->context(), name, true),
                     ".str");
    auto valPtr = llvm::ConstantExpr::getBitCast(
                    val,
                    llvm::IntegerType::getInt8PtrTy(irs->context()));
    llvm::Constant* fields[] = {
      llvm::ConstantExpr::getBitCast(
        nameVar,
        llvm::Type::getInt8PtrTy(irs->context())),
      valPtr,
    };
    elements.push_back(llvm::ConstantStruct::get(types.symListElemType, fields));
  }
  auto arrayType = llvm::ArrayType::get(types.symListElemType, elements.size());
  auto arrVar = new llvm::GlobalVariable(
                  irs->module,
                  arrayType,
                  true,
                  llvm::GlobalValue::PrivateLinkage,
                  llvm::ConstantArray::get(arrayType, elements),
                  ".str");
  return std::make_pair(
        getArrayPtr(arrVar),
        llvm::ConstantInt::get(irs->context(),APInt(32, elements.size())));
}

llvm::GlobalVariable *generateModuleListElem(IRState *irs,
                                             const Types &types,
                                             llvm::GlobalVariable *irData,
                                             llvm::GlobalVariable *irDataLen,
                                             const GlobalValsMap &globalVals) {
  assert(nullptr != irs);
  auto elem_type = types.modListElemType;
  auto funcListInit = generateFuncList(irs, types);
  auto symListInit = generateSymList(irs, types, globalVals);
  llvm::Constant* fields[] = {
    llvm::ConstantPointerNull::get(llvm::dyn_cast<llvm::PointerType>(elem_type->getElementType(0))), // next
    irData->getInitializer(), // irdata
    irDataLen->getInitializer(), // irdata len
    funcListInit.first, // funclist
    funcListInit.second, // funclist len
    symListInit.first, // symlist
    symListInit.second, // symlist len
  };

  auto init = llvm::ConstantStruct::get(elem_type, fields);

  return new llvm::GlobalVariable(
        irs->module,
        elem_type,
        false,
        llvm::GlobalValue::PrivateLinkage,
        init,
        ".rtcompile_modlist_elem");
}

llvm::PointerType *getModListHeadType(llvm::LLVMContext &context,
                                      const Types &types) {
  (void)types;
  return llvm::IntegerType::getInt8PtrTy(context);
}

llvm::GlobalVariable *declareModListHead(llvm::Module &module,
                                         const Types &types) {
  auto type = getModListHeadType(module.getContext(), types);
//  auto existingVar = module.getGlobalVariable(RuntimeCompileModulesHeadName);
//  if (nullptr != existingVar) {
//    if (type != existingVar->getType()) {
//      error(Loc(), "Invalid RuntimeCompileModulesHeadName type");
//      fatal();
//    }
//    return existingVar;
//  }
  return new llvm::GlobalVariable(
        module,
        type,
        false,
        llvm::GlobalValue::ExternalLinkage,
        nullptr,
        RuntimeCompileModulesHeadName);
}

void generateCtorBody(IRState *irs,
                      const Types &types,
                      llvm::Function *func,
                      llvm::Value *modListElem) {
  assert(nullptr != irs);
  assert(nullptr != func);
  assert(nullptr != modListElem);

  auto bb = llvm::BasicBlock::Create(irs->context(), "", func);

  llvm::IRBuilder<> builder(irs->context());
  builder.SetInsertPoint(bb);

  auto zero64 = llvm::ConstantInt::get(irs->context(), APInt(64, 0));
  auto zero32 = llvm::ConstantInt::get(irs->context(), APInt(32, 0));
  auto modListHeadPtr = declareModListHead(irs->module, types);
  llvm::Value* gepVals[] = {zero64, zero32};
  auto elemNextPtr = builder.CreateGEP(modListElem, gepVals);
  auto prevHeadVal = builder.CreateLoad(builder.CreateBitOrPointerCast(modListHeadPtr, types.modListElemType->getPointerTo()->getPointerTo()));
  auto voidPtr = builder.CreateBitOrPointerCast(modListElem, llvm::IntegerType::getInt8PtrTy(irs->context()));
  builder.CreateStore(voidPtr, modListHeadPtr);
  builder.CreateStore(prevHeadVal, elemNextPtr);

  builder.CreateRetVoid();
}

void setupModuleCtor(IRState *irs,
                     llvm::GlobalVariable *irData,
                     llvm::GlobalVariable *irDataLen,
                     const GlobalValsMap &globalVals) {
  assert(nullptr != irs);
  assert(nullptr != irData);
  assert(nullptr != irDataLen);
  Types types(irs->context());
  auto modListElem = generateModuleListElem(irs,
                                            types,
                                            irData,
                                            irDataLen,
                                            globalVals);
  auto runtimeCompiledCtor = llvm::Function::Create(
                               llvm::FunctionType::get(llvm::Type::getVoidTy(irs->context()), false),
                               llvm::GlobalValue::InternalLinkage,
                               ".rtcompile_ctor",
                               &irs->module);
  generateCtorBody(irs, types, runtimeCompiledCtor, modListElem);
  llvm::appendToGlobalCtors(irs->module, runtimeCompiledCtor, 0);
}

void setupModuleBitcodeData(const llvm::Module &srcModule,
                            IRState *irs,
                            const GlobalValsMap &globalVals) {
  assert(nullptr != irs);

  llvm::SmallString<1024> str;
  llvm::raw_svector_ostream os(str);
  llvm::WriteBitcodeToFile(&srcModule, os);

  auto runtimeCompiledIr = new llvm::GlobalVariable(
                             irs->module,
                             llvm::Type::getInt8PtrTy(irs->context()),
                             true,
                             llvm::GlobalValue::PrivateLinkage,
                             nullptr,
                             ".rtcompile_ir");

  auto runtimeCompiledIrSize = new llvm::GlobalVariable(
                                 irs->module,
                                 llvm::IntegerType::get(irs->context(), 32),
                                 true,
                                 llvm::GlobalValue::PrivateLinkage,
                                 nullptr,
                                 ".rtcompile_irsize");

  createStaticArray(
        irs->module,
        runtimeCompiledIr,
        runtimeCompiledIrSize,
        llvm::ArrayRef<uint8_t>(reinterpret_cast<uint8_t*>(str.data()), str.size()));


  setupModuleCtor(irs, runtimeCompiledIr, runtimeCompiledIrSize, globalVals);
}

llvm::Function *duplicateFunc(llvm::Module &module,
                              const llvm::Function *src) {
  assert(nullptr != src);
  auto ret = llvm::Function::Create(src->getFunctionType(),
                                    llvm::GlobalObject::ExternalLinkage,
                                    src->getName() + "__rtcomp_thunk__",
                                    &module);
  ret->setCallingConv(src->getCallingConv());
  ret->setAttributes(src->getAttributes());
  return ret;
}

llvm::Function *createThunkFunc(llvm::Module &module,
                                const llvm::Function *src,
                                llvm::GlobalVariable *thunkVar) {
  assert(nullptr != src);
  assert(nullptr != thunkVar);
  auto func = duplicateFunc(module, src);

  auto bb = llvm::BasicBlock::Create(module.getContext(), "", func);
  llvm::IRBuilder<> builder(module.getContext());
  builder.SetInsertPoint(bb);
  auto thunkPtr = builder.CreateLoad(thunkVar);
  llvm::SmallVector<llvm::Value*, 6> args;
  for(auto& arg: func->args()) {
    args.push_back(&arg);
  }
  auto ret = builder.CreateCall(thunkPtr, args);
  if (func->getReturnType()->isVoidTy()) {
    builder.CreateRetVoid();
  }
  else {
    builder.CreateRet(ret);
  }
  return func;
}

} // anon namespace

void generateBitcodeForRuntimeCompile(IRState *irs) {
  assert(nullptr != irs);
  if (irs->runtimeCompiledFunctions.empty()) {
    return;
  }
  auto filter = createGlobalValsFilter(irs);

  llvm::ValueToValueMapTy unused;
  auto newModule = llvm::CloneModule(&irs->module,
                                     unused,
                                     [&](const llvm::GlobalValue *val)->bool {
    // We don't dereference, so const_cast should be safe
    return (filter.end() != filter.find(const_cast<llvm::GlobalValue*>(val)));
  });
  fixupRtThunks(*newModule, irs->runtimeCompiledFunctions);
  //hideExternalSymbols(*newModule, filter);

  //newModule->dump();
  setupModuleBitcodeData(*newModule, irs, filter);
}

void addRuntimeCompiledFunction(IRState *irs, IrFunction *func) {
  assert(nullptr != irs);
  assert(nullptr != func);
  assert(nullptr != func->func);
  if (!global.params.enableRuntimeCompile) {
    return;
  }
  auto srcFunc = func->func;
  auto thunkVarType = srcFunc->getFunctionType()->getPointerTo();
  auto thunkVar = new llvm::GlobalVariable(irs->module,
                                           thunkVarType,
                                           false,
                                           llvm::GlobalValue::PrivateLinkage,
                                           llvm::ConstantPointerNull::get(thunkVarType),
                                           ".rtcompile_thunkvar_" + srcFunc->getName());
  auto thunkFunc = createThunkFunc(irs->module, srcFunc, thunkVar);
  func->rtCompileFunc = thunkFunc;
  irs->runtimeCompiledFunctions.push_back(
        IRState::RtCompiledFuncDesc{srcFunc, thunkVar, thunkFunc});
}
