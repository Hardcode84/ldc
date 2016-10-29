extern "C" {
void* runtimecompile_modules_head = nullptr;
#ifdef _WIN32
__declspec(dllimport)
#endif
extern void rtCompileProcessImplSo(const void* modlist_head);

void rtCompileProcessImpl()
{
  rtCompileProcessImplSo(runtimecompile_modules_head);
}
}
