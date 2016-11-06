
// RUN: %ldc -enable-runtime-compile -run %s

import std.exception;
import ldc.runtimecompile;

@runtimeCompile void foo()
{
  throw new Exception("foo");
}

void main(string[] args)
{
  rtCompileProcess();
  assert(collectExceptionMsg(foo()) == "foo");
}
