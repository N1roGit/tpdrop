/// Simple program to test whether running a user program works.
///
/// Just do a “syscall” that shuts down the OS.
///
/// NOTE: for some reason, user programs with global data structures
/// sometimes have not worked in the Nachos environment.  So be careful out
/// there!  One option is to allocate data structures as automatics within a
/// procedure, but if you do this, you have to be careful to allocate a big
/// enough stack to hold the automatics!

#include "syscall.h"

int
main(int argc, char *argv[])
{
    char c[1];
    int i = 0; //con 5 anda por algun motivo
    if (argc == 2){
    for(;argv[1][i] != 'q';i++){
        c[1] = argv[1][i];

        if((int)c[1]<32){
            c[1] = (char)(((int)c[1]) + 48 );
            Write(c,1,CONSOLE_OUTPUT);
            Write(">>\n",4 ,CONSOLE_OUTPUT);
        }
        else
        Write(&(argv[1][i]),1 ,CONSOLE_OUTPUT);
    }
    Write("\n",1 ,CONSOLE_OUTPUT);
}
}