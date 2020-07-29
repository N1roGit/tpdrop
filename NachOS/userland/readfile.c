#include "syscall.h"

int
main(void)
{
    OpenFileId o = Open("test.txt");
    char* c;
    Read(c,10,o);
    Write(c,10,CONSOLE_OUTPUT);
    Close(o);
    Halt();
}
