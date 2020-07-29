/// Copyright (c) 2019-2020 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "transfer.hh"
#include "lib/utility.hh"
#include "threads/system.hh"


void ReadBufferFromUser(int userAddress, char *outBuffer,
                        unsigned byteCount)
{
    ASSERT(userAddress != 0);
    ASSERT(outBuffer != nullptr);
    ASSERT(byteCount != 0);
    for(;byteCount-- > 0;outBuffer++){
        for(int i = 0 ;  (machine->ReadMem(userAddress,1,(int*)outBuffer))==false &&  i<=4 ; i++);
    }
    userAddress++;
    DEBUG('B', "Lei %s \n", outBuffer);
}

bool ReadStringFromUser(int userAddress, char *outString,
                        unsigned maxByteCount)
{
    ASSERT(userAddress != 0);
    ASSERT(outString != nullptr);
    ASSERT(maxByteCount != 0);

    unsigned count = 0;
    do {
        int temp;
        count++;

        for(int i = 0;(machine->ReadMem(userAddress, 1, &temp))==false && i <= 4; i++){
            DEBUG('v',"Fallo %d veces\n",i);
        }
        *outString = (unsigned char) temp;
        userAddress++;
    } while (*outString++ != '\0' && count < maxByteCount);

    return *(outString - 1) == '\0';
}

void WriteBufferToUser(const char *buffer, int userAddress,
                       unsigned byteCount)
{
    ASSERT(userAddress != 0);
    ASSERT(buffer != nullptr);
    ASSERT(byteCount != 0);
    for(;byteCount-- > 0;buffer++){
        for(int i = 0;machine->WriteMem(userAddress, 1, *buffer)==false && i<=4; i++){
            DEBUG('v',"Fallo %d veces\n",i);
        }
    userAddress++;
    DEBUG('B', "Escribi %s \n", buffer);
}
}

void WriteStringToUser(const char *string, int userAddress)
{
    ASSERT(userAddress != 0);
    ASSERT(string != nullptr);
    do {
        for(int i = 0;!(machine->WriteMem(userAddress, 1, *string)) && i <= 4 ; i++ ){
            DEBUG('v',"Fallo %d veces\n",i);            
        }
            userAddress++;
    } while (*string++ != '\0');
}
