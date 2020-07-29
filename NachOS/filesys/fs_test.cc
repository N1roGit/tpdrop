/// Simple test routines for the file system.
///
/// We implement:
///
/// Copy
///     Copy a file from UNIX to Nachos.
/// Print
///     Cat the contents of a Nachos file.
/// Perftest
///     A stress test for the Nachos file system read and write a really
///     really large file in tiny chunks (will not work on baseline system!)
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2016-2020 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "file_system.hh"
#include "lib/utility.hh"
#include "machine/disk.hh"
#include "machine/statistics.hh"
#include "threads/thread.hh"
#include "threads/system.hh"

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>


static const unsigned TRANSFER_SIZE = 10;  // Make it small, just to be
                                           // difficult.

/// Copy the contents of the UNIX file `from` to the Nachos file `to`.
void
Copy(const char *from, const char *to)
{
    ASSERT(from != nullptr);
    ASSERT(to != nullptr);

    // Open UNIX file.
    FILE *fp = fopen(from, "r");
    if (fp == nullptr) {
        printf("Copy: could not open input file %s\n", from);
        return;
    }

    // Figure out length of UNIX file.
    fseek(fp, 0, 2);
    int fileLength = ftell(fp);
    fseek(fp, 0, 0);

    DEBUG('f', "Copying file %s, size %u, to file %s\n",
          from, fileLength, to);

    // Create a Nachos file of the same length.
    if (!fileSystem->Create(to, fileLength)) {  // Create Nachos file.
        printf("Copy: could not create output file %s\n", to);
        fclose(fp);
        return;
    }
    DEBUG('f', "Created file %s, now opening\n",to);
    OpenFile *openFile = fileSystem->Open(to);
    ASSERT(openFile != nullptr);

    // Copy the data in `TRANSFER_SIZE` chunks.
    char *buffer = new char [TRANSFER_SIZE];
    int amountRead;
    while ((amountRead = fread(buffer, sizeof(char),
                               TRANSFER_SIZE, fp)) > 0)
        openFile->Write(buffer, amountRead);
    delete [] buffer;

    // Close the UNIX and the Nachos files.
    delete openFile;
    fclose(fp);
}

/// Print the contents of the Nachos file `name`.
void
Print(const char *name)
{
    ASSERT(name != nullptr);

    OpenFile *openFile = fileSystem->Open(name);
    if (openFile == nullptr) {
        fprintf(stderr, "Print: unable to open file %s\n", name);
        return;
    }

    char *buffer = new char [TRANSFER_SIZE];
    int amountRead;
    while ((amountRead = openFile->Read(buffer, TRANSFER_SIZE)) > 0)
        for (unsigned i = 0; i < (unsigned) amountRead; i++)
            printf("%c", buffer[i]);

    delete [] buffer;
    delete openFile;  // close the Nachos file
}


/// Performance test
///
/// Stress the Nachos file system by creating a large file, writing it out a
/// bit at a time, reading it back a bit at a time, and then deleting the
/// file.
///
/// Implemented as three separate routines:
/// * `FileWrite` -- write the file.
/// * `FileRead` -- read the file.
/// * `PerformanceTest` -- overall control, and print out performance #'s.
/*
static const char FILE_NAME[] = "TestFile";
static const char CONTENTS[] = "1234567890";
static const unsigned CONTENT_SIZE = sizeof CONTENTS - 1;
static const unsigned FILE_SIZE = CONTENT_SIZE * 10; // 5000 ERA EL DEFAULT
*/

char FILE_NAME[] = "TestFile";
char CONTENTS[] = "SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS"; 
unsigned CONTENT_SIZE = sizeof CONTENTS - 1;
unsigned FILE_SIZE = 100000;//90000; //CONTENT_SIZE * 6000; // 5000 ERA EL DEFAULT

static void
FileWrite()
{
    printf("Sequential write of %u byte file, in %u byte chunks\n",
           FILE_SIZE, CONTENT_SIZE);

    if (!fileSystem->Create(FILE_NAME,FILE_SIZE)) {
        fprintf(stderr, "Perf test: cannot create %s\n", FILE_NAME);
        return;
    }

    OpenFile *openFile = fileSystem->Open(FILE_NAME);
    if (openFile == nullptr) {
        fprintf(stderr, "Perf test: unable to open %s\n", FILE_NAME);
        return;
    }

    for (unsigned i = 0; i < FILE_SIZE; i += CONTENT_SIZE) {
        int numBytes = openFile->Write(CONTENTS, CONTENT_SIZE);
        if (numBytes < 20) { //volver a poner a 10 despues del Ftest
            fprintf(stderr, "Perf test: unable to write %s\n", FILE_NAME);
            break;
        }
    }
    delete openFile;
}

static void
FileRead()
{
    printf("Sequential read of %u byte file, in %u byte chunks\n",
           FILE_SIZE, CONTENT_SIZE);

    OpenFile *openFile = fileSystem->Open(FILE_NAME);
    if (openFile == nullptr) {
        fprintf(stderr, "Perf test: unable to open file %s\n", FILE_NAME);
        return;
    }

    char *buffer = new char [CONTENT_SIZE];
    for (unsigned i = 0; i < FILE_SIZE; i += CONTENT_SIZE) {
        int numBytes = openFile->Read(buffer, CONTENT_SIZE);
        if (numBytes < 10 || strncmp(buffer, CONTENTS, CONTENT_SIZE)){
            printf("Perf test: unable to read %s\n", FILE_NAME);
            break;
        }
    }

    delete [] buffer;
    delete openFile;
}

void
PerformanceTest()
{
    printf("Starting file system performance test:\n");
    stats->Print();
    FileWrite();
    FileRead();
    if (!fileSystem->Remove(FILE_NAME)) {
        printf("Perf test: unable to remove %s\n", FILE_NAME);
        return;
    }
    stats->Print();
}





static void
FileWrite2(void *n)
{
    sleep(rand()%5);
    //printf("Escritor %s\n",currentThread->GetName());
    //printf("Sequential write of %u byte file, in %u byte chunks\n",
     //      FILE_SIZE, CONTENT_SIZE);

    /*if (!fileSystem->Create(FILE_NAME,FILE_SIZE)) {
        fprintf(stderr, "Perf test: cannot create %s\n", FILE_NAME);
        return;
    }*/

    OpenFile *openFile = fileSystem->Open(FILE_NAME);
    if (openFile == nullptr) {
        fprintf(stderr, "Perf test: unable to open %s\n", FILE_NAME);
        return;
    }

    for (unsigned i = 0; i < FILE_SIZE; i += CONTENT_SIZE) {
        //syncmap->PrintAll();
        int numBytes = openFile->Write(currentThread->GetName(), CONTENT_SIZE);
        //syncmap->PrintAll();
        if (numBytes < 1) {
            fprintf(stderr, "Perf test: unable to write %s\n", FILE_NAME);
            break;
        }
    }
    delete openFile;
}


static void
FileRead2(void* n)
{
    sleep(rand()%5);
    //printf("Lector %s\n",(char*)n);
   // printf("Sequential read of %u byte file, in %u byte chunks\n",
    //       FILE_SIZE, CONTENT_SIZE);

    OpenFile *openFile = fileSystem->Open(FILE_NAME);
    if (openFile == nullptr) {
        fprintf(stderr, "Perf test: unable to open file %s , %s \n", FILE_NAME,(char*)n );
        return;
    }

    char *buffer = new char [CONTENT_SIZE];
    for (unsigned i = 0; i < FILE_SIZE; i += CONTENT_SIZE) {
        //syncmap->PrintAll();
        int numBytes = openFile->Read(buffer, CONTENT_SIZE);
        if (numBytes < 1){// || strncmp(buffer, CONTENTS, CONTENT_SIZE)){
            printf("Perf test: unable to read %s\n", FILE_NAME);
            break;
        }
        printf("%s leyo %c\n",(char*)n,buffer[0]);
        
    }

    delete [] buffer;
    delete openFile;
}

void StatusTest(void *n){
    //sleep(8);
    //fileSystem->Remove(FILE_NAME);
}


void Ftest(){
    FileWrite();
    /*for(int i = 0; i<8 ;i = i+1){
        if(rand()%(i+1) == 0){
        char *name = new char [64];
        sprintf(name, "%c", 'B'  + i);
        Thread *newThread2 = new Thread(name,true,4);
        newThread2->Fork(FileWrite2, (void *) name);
        }
        else{
        char *name = new char [64];
        sprintf(name, "%c", 'b'  + i);
        Thread *newThread = new Thread(name,true,4);
        newThread->Fork(FileRead2, (void *) name);
        }
    }*/
    //Thread *t = new Thread("name",true,4);
    //t->Fork(StatusTest,(void *) "name");
    
    for(int i = 0; i<4 ;i = i+1)
    {
    char *name = new char [64];
        sprintf(name, "%c", 'A'  + i);
        Thread *newThread2 = new Thread(name,true,4);
        newThread2->Fork(FileWrite2, (void *) name);
    }
    for(int i = 0; i<4 ;i = i+1)
    {
        char *name = new char [64];
        sprintf(name, "%c", 'a'  + i);
        Thread *newThread = new Thread(name,true,4);
        newThread->Fork(FileRead2, (void *) name);
    }

    
}

void
ExpandTest2()
{
    printf("Sequential write of %u byte file, in %u byte chunks\n",
           1, 1);

    if (!fileSystem->Create("ExpandTest",1)) {
        fprintf(stderr, "Perf test: cannot create\n");
        return;
    }

    OpenFile *openFile = fileSystem->Open("ExpandTest");
    if (openFile == nullptr) {
        fprintf(stderr, "Perf test: unable to open\n");
        return;
    }

    for (unsigned i = 0; i < 2; i += 1) {
        int numBytes = openFile->Write("A", 1);
        if (numBytes < 20) { //volver a poner a 10 despues del Ftest
            //fprintf(stderr, "Perf test: unable to write %s\n", FILE_NAME);
            //break;
        }
    }

    printf("Exiting.......\n");
    delete openFile;
    Print("ExpandTest");
    printf("\n");
}

void
ExpandTest()
{
    printf("Sequential write of %u byte file, in %u byte chunks\n",
           1, 10);

    if (!fileSystem->Create("ExpandTest",1)) {
        fprintf(stderr, "Perf test: cannot create\n");
        return;
    }

    OpenFile *openFile = fileSystem->Open("ExpandTest");
    if (openFile == nullptr) {
        fprintf(stderr, "Perf test: unable to open\n");
        return;
    }

    for (unsigned i = 0; i < 4000; i += 10) {
        int numBytes = openFile->Write("AAAAAAAAAA", 10);
        if (numBytes < 20) { //volver a poner a 10 despues del Ftest
            //fprintf(stderr, "Perf test: unable to write %s\n", FILE_NAME);
            //break;
        }
    }

    printf("Exiting.......\n");
    delete openFile;
    Print("ExpandTest");
    printf("\n");
}