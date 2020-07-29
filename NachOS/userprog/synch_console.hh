

#ifndef NACHOS_FILESYS_SYNCHCONSOLE__HH
#define NACHOS_FILESYS_SYNCHCONSOLE__HH


#include "machine/console.hh"
#include "threads/synch.hh"


class SynchConsole {
public:

    SynchConsole();

    ~SynchConsole();

    void PutChar(char ch);

    char GetChar();
    //sino se rompe
    void WriteDone();
    void ReadAvail();


private:
    Console *console; 
    Lock *reading;  
    Lock *writing;  
    Semaphore *readAvail;
    Semaphore *writeDone;
};


#endif
