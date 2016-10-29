module ldc.runtimecompile;

immutable runtimeCompile = _runtimeCompile();
private struct _runtimeCompile {}

void rtCompileProcess()
{
  rtCompileProcessImpl();
}

private:
extern(C)
{
extern void rtCompileProcessImpl();
}