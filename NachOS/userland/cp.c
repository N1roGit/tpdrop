#include "syscall.h"

void main(int argc, char** argv) {
    int origen, destino;   
    char c[1];
    if (argc != 3)
        Write("Args incorrectas.\n", 18, CONSOLE_OUTPUT);
    else if ((origen = Open(argv[1])) == -1)
        Write("No se pudo abrir archivo de origen.\n", 38, CONSOLE_OUTPUT);
    else {
        Create(argv[2]);
        if((destino = Open(argv[2])) == -1){
            Write("No se pudo abrir archivo de destino.\n", 39, CONSOLE_OUTPUT);
        }
        else{
            while (Read(c, 1, origen)) Write(c, 1, destino);
            Close(origen);
            
            Close(destino);
        }
    }

    Exit(0);
}
