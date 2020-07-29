#include "syscall.h"

int
main(void)
{
    Exec("readfile", 0, 1);
    Exec("writefile", 0, 1);
    Exec("writefile", 0, 1);
    Exec("readfile", 0, 1);

    Write("This should be printed.\n", 28, CONSOLE_OUTPUT);
    Halt();
}
