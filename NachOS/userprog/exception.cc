/// Entry points into the Nachos kernel from user programs.
///
/// There are two kinds of things that can cause control to transfer back to
/// here from user code:
///
/// * System calls: the user code explicitly requests to call a procedure in
///   the Nachos kernel.  Right now, the only function we support is `Halt`.
///
/// * Exceptions: the user code does something that the CPU cannot handle.
///   For instance, accessing memory that does not exist, arithmetic errors,
///   etc.
///
/// Interrupts (which can also cause control to transfer from user code into
/// the Nachos kernel) are handled elsewhere.
///
/// For now, this only handles the `Halt` system call.  Everything else core-
/// dumps.
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2016-2020 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "transfer.hh"
#include "syscall.h"
#include "filesys/directory_entry.hh"
#include "threads/system.hh"
#include "threads/thread.hh"
#include "synch_console.hh"
#include "threads/system.hh"
#include "userprog/args.hh"

#include <stdio.h>


static void
IncrementPC()
{
    unsigned pc;

    pc = machine->ReadRegister(PC_REG);
    machine->WriteRegister(PREV_PC_REG, pc);
    pc = machine->ReadRegister(NEXT_PC_REG);
    machine->WriteRegister(PC_REG, pc);
    pc += 4;
    machine->WriteRegister(NEXT_PC_REG, pc);
}

/// Do some default behavior for an unexpected exception.
///
/// NOTE: this function is meant specifically for unexpected exceptions.  If
/// you implement a new behavior for some exception, do not extend this
/// function: assign a new handler instead.
///
/// * `et` is the kind of exception.  The list of possible exceptions is in
///   `machine/exception_type.hh`.
static void
DefaultHandler(ExceptionType et)
{
    int exceptionArg = machine->ReadRegister(2);

    fprintf(stderr, "Unexpected user mode exception: %s, arg %d.\n",
            ExceptionTypeToString(et), exceptionArg);
    ASSERT(false);
}


void ForkInit(void* args_) { // a)
    currentThread->space->InitRegisters();
    currentThread->space->RestoreState();

    if (args_) {
        char **args = (char**) args_;
        int wargs = WriteArgs(args);
        machine->WriteRegister(4, wargs);
        DEBUG('A', "args = %d.\n",wargs);
        int stack =  machine->ReadRegister(STACK_REG) ;
        DEBUG('A', "argv en = %X.\n",stack);
        machine->WriteRegister(5,stack +16);

    machine->Run();
    }
}


/// Handle a system call exception.
///
/// * `et` is the kind of exception.  The list of possible exceptions is in
///   `machine/exception_type.hh`.
///
/// The calling convention is the following:
///
/// * system call identifier in `r2`;
/// * 1st argument in `r4`;
/// * 2nd argument in `r5`;
/// * 3rd argument in `r6`;
/// * 4th argument in `r7`;
/// * the result of the system call, if any, must be put back into `r2`.
///
/// And do not forget to increment the program counter before returning. (Or
/// else you will loop making the same system call forever!)
static void
SyscallHandler(ExceptionType _et)
{
    int scid = machine->ReadRegister(2); //v0




    switch (scid) {

        case SC_HALT:
            DEBUG('e', "Shutdown, initiated by user program.\n");
            interrupt->Halt();
            break;

        case SC_CREATE: {
            machine->WriteRegister(2,-1);
            int filenameAddr = machine->ReadRegister(4); //a0
            if (filenameAddr == 0){
                DEBUG('e', "Error: address to filename string is null.\n");
                 machine->WriteRegister(2,-1);//v0
                 break;
            }

            char filename[FILE_NAME_MAX_LEN + 1];
            if (!ReadStringFromUser(filenameAddr, filename, sizeof filename)){
                DEBUG('e', "Error: filename string too long (maximum is %u bytes).\n",
                      FILE_NAME_MAX_LEN);
                machine->WriteRegister(2,-1);//v0
                break;
            }

            DEBUG('e', "`Create` requested for file `%s`.\n", filename);
            if(fileSystem->Create(filename,1000))machine->WriteRegister(2,0);
            break;
        }
        case SC_REMOVE: {
            machine->WriteRegister(2,-1);
            int filenameAddr = machine->ReadRegister(4); //a0
            if (filenameAddr == 0){
                DEBUG('e', "Error: address to filename string is null.\n");
                 machine->WriteRegister(2,-1);//v0
                 break;
            }

            char filename[FILE_NAME_MAX_LEN + 1];
            if (!ReadStringFromUser(filenameAddr, filename, sizeof filename)){
                DEBUG('e', "Error: filename string too long (maximum is %u bytes).\n",
                      FILE_NAME_MAX_LEN);
                machine->WriteRegister(2,-1);//v0
                break;
            }

            DEBUG('e', "`Remove` requested for file `%s`.\n", filename);
            if(fileSystem->Remove(filename))machine->WriteRegister(2,0);
            break;
        }
        case SC_OPEN:{
            int filenameAddr = machine->ReadRegister(4); //a0
            if (filenameAddr == 0){
                machine->WriteRegister(2,-1);//v0
                DEBUG('e', "Error: address to filename string is null.\n");
                break;
                }

            char filename[FILE_NAME_MAX_LEN + 1];
            if (!ReadStringFromUser(filenameAddr, filename, sizeof filename)){
                DEBUG('e', "Error: filename string too long (maximum is %u bytes).\n",
                      FILE_NAME_MAX_LEN);         
                 machine->WriteRegister(2,-1);//v0
                 break;
            }
            else
            {
                DEBUG('e', "`Open` requested for file `%s` size %d.\n", filename,sizeof filename);
                OpenFile* open_file = fileSystem->Open(filename);
                if(open_file != nullptr){
                    int id = currentThread->FileAdd(open_file);
                    if(id != -1)machine->WriteRegister(2,-1);;
                    DEBUG('e', "Opened file `%s`.\n", filename);
                    machine->WriteRegister(2,id);//v0
                }
                else{
                    DEBUG('e', "File `%s` not found.\n", filename);
                    machine->WriteRegister(2,-1);//v0
                }
            }
            break;
        }

        case SC_CLOSE: {
            int fid = machine->ReadRegister(4); //a0 viene con el openfile ID
            DEBUG('e', "`Close` requested for id %u.\n", fid);
            if (currentThread->FileIsEmpty() || !(currentThread->FileHasKey(fid))){
                DEBUG('e', "No such file to close (file id: `%d`).\n", fid);
                machine->WriteRegister(2,-1); //v0
                break;
            }
            else{
                OpenFile* file =  currentThread->FileRemove(fid);
                delete file; //con el delete se cierra el openfile
                DEBUG('e', "Closed file id `%d`.\n", fid);
                machine->WriteRegister(2,0); //v0
            }
            break;
        }

        case SC_WRITE:{
            int string = machine->ReadRegister(4); //a0 viene con el buffer
            int string_sz = machine->ReadRegister(5); //a1 viene con el tamaño
            int fid = machine->ReadRegister(6); //a2 viene con el openfile ID

            char buffer[string_sz+1];
            
            ReadStringFromUser(string,buffer,unsigned(string_sz));


            DEBUG('e', "`Write` requested for id %u (Wants to write '%s').\n", fid,buffer);

            if(buffer == nullptr || string_sz<0){
                DEBUG('e', "Sring null or bad size for file id: `%d` size %d.\n", fid,string_sz);
                machine->WriteRegister(2,-1); //v0
                break;
            }

            if(fid>1){ //escribir en un archivo
                if(!currentThread->FileHasKey(fid)){
                    DEBUG('e', "No such file to write (file id: `%d`).\n", fid);
                    machine->WriteRegister(2,-1); //v0
                    break;
                }

                OpenFile *file = currentThread->FileGet(fid);
                int status = file->Write(buffer, (unsigned)string_sz);
                DEBUG('e', "Write for file id: `%d` completed with status %d .\n", fid,status);
                machine->WriteRegister(2,status); //v0
            }
            else if(fid == 1){//escribir por consola
                unsigned int x = 0;
                for(;x<(unsigned)string_sz;x++){
                    consola_sinc->PutChar(buffer[x]);
                }
            }

            break;
        }
        case SC_READ:
        { //read size items from openfile into buffer
            int string = machine->ReadRegister(4); //a0 viene con el buffer
            int string_sz = machine->ReadRegister(5); //a1 viene con el tamaño
            int fid = machine->ReadRegister(6); //a2 viene con el openfile ID
            char buffer[string_sz+1];
            //WriteStringFromUser(string,buffer,unsigned(string_sz));
            


            DEBUG('e', "`Read` requested for id %u.\n", fid);

            if(string == 0 || string_sz<0){
                DEBUG('e', "Sring bad address or bad size for file id: `%s` size %d.\n", fid,string_sz);
                machine->WriteRegister(2,-1); //v0
                break;
            }


            if(fid>1){ //LEER en un archivo
                if(!currentThread->FileHasKey(fid)){
                    DEBUG('e', "No such file to write (file id: `%s`).\n", fid);
                    machine->WriteRegister(2,-1); //v0
                    break;
                }
                OpenFile *file = currentThread->FileGet(fid);
                int status = file->Read(buffer,string_sz); // con readAt 0 lo leo desde el principio, asi lo leo desde donde quedo
            
                WriteBufferToUser(buffer,string,string_sz);
                DEBUG('e', "Read for file id: `%d` completed with status %d (data: %s) .\n", fid,status,buffer);
                machine->WriteRegister(2,status); //v0
            }
            else if(fid == 0){//LEER por consola
                unsigned int x = 0;
                for(;x<(unsigned)string_sz;x++){
                   buffer[x]=consola_sinc->GetChar();
                }
                WriteBufferToUser(buffer,string,string_sz);
            
            }

            break;
        }

        case SC_EXIT: {
            int status = machine->ReadRegister(4);
            DEBUG('e', "Exiting with status: %d.\n", status);
            currentThread->Finish(status);
            break;
        }


        case SC_EXEC:{
            int filenameAddr = machine->ReadRegister(4);
            int argvAddr = machine->ReadRegister(5);
            int enableJoin = machine->ReadRegister(6);

            if (filenameAddr == 0){
                DEBUG('e', "Error: address to filename string is null.\n");
                machine -> WriteRegister(2, -1);
                break;
            }

            char filename[FILE_NAME_MAX_LEN + 1];
            if (!ReadStringFromUser(filenameAddr, filename, sizeof filename)){
                DEBUG('e', "Error: filename string too long (maximum is %u bytes).\n",
                      FILE_NAME_MAX_LEN);
                machine -> WriteRegister(2, -1);
                break;
            }

            OpenFile *filePtr = fileSystem -> Open(filename);
            if(filePtr == nullptr){
                DEBUG('e', "Error: file %s not found.\n", filename);
                machine -> WriteRegister(2, -1);
                break;
            }


            Thread *newThread = new Thread(filename, bool(enableJoin),currentThread->get_priority());
            SpaceId newSpaceId = newThread -> pid;
            AddressSpace *newAddressSpace = new AddressSpace(filePtr,newSpaceId);


            newThread -> space = newAddressSpace;


            if(argvAddr == 0)
                newThread -> Fork(ForkInit, nullptr);
            else
                newThread -> Fork(ForkInit, SaveArgs(argvAddr));

            //delete filePtr;

            machine -> WriteRegister(2, newSpaceId);

            break;
        }


        case SC_JOIN: {
            SpaceId spaceId = machine -> ReadRegister(4);

            if(spaceId < 0){
                DEBUG('e', "Error: Invalid spaceId.\n");
                machine -> WriteRegister(2, -1);
                break;
            }

            if(not spaceIds -> HasKey(spaceId)){
                DEBUG('e', "Error: Thread with id %d not found.\n", spaceId);
                machine -> WriteRegister(2, -1);
                break;
            }

            Thread *threadToJoin = spaceIds -> Get(spaceId);

            DEBUG('e', "Requested Join with SpaceId %d\n", spaceId);
            int exitStatus = threadToJoin -> Join();

            machine -> WriteRegister(2, exitStatus);
            break;
        }
        

        default:
            fprintf(stderr, "Unexpected system call: id %d.\n", scid);
            ASSERT(false);

    }

    IncrementPC();
}

static void
ReadOnlyHandler(ExceptionType et){

    currentThread->Finish(et);
}



static void
PageFaultHandler(ExceptionType et)
{
    stats -> numPageFaults ++;

    unsigned virtualAddress = machine->ReadRegister(BAD_VADDR_REG);

    currentThread->space->Update(virtualAddress);

}

/// By default, only system calls have their own handler.  All other
/// exception types are assigned the default handler.
void
SetExceptionHandlers()
{
    machine->SetHandler(NO_EXCEPTION,            &DefaultHandler);
    machine->SetHandler(SYSCALL_EXCEPTION,       &SyscallHandler);
    machine->SetHandler(PAGE_FAULT_EXCEPTION,    &DefaultHandler);
    machine->SetHandler(READ_ONLY_EXCEPTION,     &DefaultHandler);
    machine->SetHandler(BUS_ERROR_EXCEPTION,     &DefaultHandler);
    machine->SetHandler(ADDRESS_ERROR_EXCEPTION, &DefaultHandler);
    machine->SetHandler(OVERFLOW_EXCEPTION,      &DefaultHandler);
    machine->SetHandler(ILLEGAL_INSTR_EXCEPTION, &DefaultHandler);
    machine->SetHandler(PAGE_FAULT_EXCEPTION,    &PageFaultHandler);
    machine->SetHandler(READ_ONLY_EXCEPTION,     &ReadOnlyHandler);
}
