#ifndef RUNTIMECOMPILE_H
#define RUNTIMECOMPILE_H

struct IRState;
struct IrFunction;

void generateBitcodeForRuntimeCompile(IRState *irs);
void addRuntimeCompiledFunction(IRState *irs, IrFunction *func);

#endif // RUNTIMECOMPILE_H
