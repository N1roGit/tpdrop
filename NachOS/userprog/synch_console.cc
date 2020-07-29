
#include "synch_console.hh"


/// Dummy functions because C++ is weird about pointers to member functions.

static void
ReadAvailDummy(void *args)
{
    ASSERT(args != nullptr);

    SynchConsole *c = (SynchConsole*) args;
    c->ReadAvail();
}

static void
WriteDoneDummy(void *args)
{
    ASSERT(args != nullptr);

    SynchConsole *c = (SynchConsole*) args;
    c->WriteDone();
}

SynchConsole::SynchConsole()
{
    console = new Console(nullptr,nullptr,ReadAvailDummy,WriteDoneDummy,this);
    reading = new Lock("Console reading");
    writing = new Lock("Console writing");
    readAvail = new Semaphore("Console readers", 0);
    writeDone = new Semaphore("Console waiters", 0);
}

SynchConsole::~SynchConsole()
{
    delete console;
    delete reading;
    delete writing;
    delete readAvail;
    delete writeDone;
}

void
SynchConsole::PutChar(char ch)
{
    DEBUG('o', "Enviando caracter `%c` a la consola.\n", ch);

    writing->Acquire();
        console->PutChar(ch);
        writeDone->P();
    writing->Release();
}

char
SynchConsole::GetChar()
{
    DEBUG('o', "Leyendo de la consola...\n");

    reading->Acquire();
        readAvail->P();
        char ch = console->GetChar();
    reading->Release();

    DEBUG('o', "Caracter `%c` leido.", ch);

    return ch;
}

void
SynchConsole::ReadAvail()
{
    readAvail->V();
}

void
SynchConsole::WriteDone()
{
    writeDone->V();
}

