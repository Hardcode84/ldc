#ifndef CONTEXT_H
#define CONTEXT_H

typedef void (*InterruptPointHandlerT)(void*, const char* action, const char* object);
typedef void (*FatalHandlerT)(void*, const char* reason);
// must be synchronized with D source
struct Context final
{
    unsigned optLevel = 0;
    unsigned sizeLeve = 0;
    InterruptPointHandlerT interruptPointHandler = nullptr;
    void* interruptPointHandlerData = nullptr;
    FatalHandlerT fatalHandler = nullptr;
    void* fatalHandlerData = nullptr;
};

#endif // CONTEXT_H
