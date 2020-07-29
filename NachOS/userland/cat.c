#include "syscall.h"

int main(int argc, char** argv) {
    int o; 
    char c[1];

    if (argc != 2)
        Write("Args incorrectos.\n", 18, CONSOLE_OUTPUT);
    else if ((o = Open(argv[1])) == -1)
        Write("No se pudo abrir el archivo.\n", 31, CONSOLE_OUTPUT);
    else{
        while (Read(c, 1, o)) Write(c,1, CONSOLE_OUTPUT);
        Write("\n",1 ,CONSOLE_OUTPUT);
    }
    Exit(0);
}
