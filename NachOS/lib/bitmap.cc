/// Routines to manage a bitmap -- an array of bits each of which can be
/// either on or off.  Represented as an array of integers.
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2016-2020 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "bitmap.hh"

#include <stdio.h>
#include <stdlib.h>


/// Initialize a bitmap with `nitems` bits, so that every bit is clear.  It
/// can be added somewhere on a list.
///
/// * `nitems` is the number of bits in the bitmap.
Bitmap::Bitmap(unsigned nitems)
{
    ASSERT(nitems > 0);

    numBits  = nitems;
    numWords = DivRoundUp(numBits, BITS_IN_WORD);
    map      = new unsigned [numWords];
    for (unsigned i = 0; i < numBits; i++)
        Clear(i);
}

/// De-allocate a bitmap.
Bitmap::~Bitmap()
{
    delete [] map;
}

/// Set the “nth” bit in a bitmap.
///
/// * `which` is the number of the bit to be set.
void
Bitmap::Mark(unsigned which)
{
    ASSERT(which < numBits);
    map[which / BITS_IN_WORD] |= 1 << which % BITS_IN_WORD;
}

/// Clear the “nth” bit in a bitmap.
///
/// * `which` is the number of the bit to be cleared.
void
Bitmap::Clear(unsigned which)
{
    ASSERT(which < numBits);
    map[which / BITS_IN_WORD] &= ~(1 << which % BITS_IN_WORD);
}

/// Return true if the “nth” bit is set.
///
/// * `which` is the number of the bit to be tested.
bool
Bitmap::Test(unsigned which) const
{
    ASSERT(which < numBits);
    return map[which / BITS_IN_WORD] & 1 << which % BITS_IN_WORD;
}

/// Return the number of the first bit which is clear.  As a side effect, set
/// the bit (mark it as in use).  (In other words, find and allocate a bit.)
///
/// If no bits are clear, return -1.
int
Bitmap::Find()
{
    for (unsigned i = 0; i < numBits; i++)
        if (!Test(i)) {
            Mark(i);
            return i;
        }
    return -1;
}

/// Return the number of clear bits in the bitmap.  (In other words, how many
/// bits are unallocated?)
unsigned
Bitmap::CountClear() const
{
    unsigned count = 0;

    for (unsigned i = 0; i < numBits; i++)
        if (!Test(i))
            count++;
    return count;
}

/// Print the contents of the bitmap, for debugging.
///
/// Could be done in a number of ways, but we just print the indexes of all
/// the bits that are set in the bitmap.
void
Bitmap::Print() const
{
    printf("Bitmap bits set:\n");
    for (unsigned i = 0; i < numBits; i++)
        if (Test(i))
            printf("%u ", i);
    printf("\n");
}

/// Initialize the contents of a bitmap from a Nachos file.
///
/// Note: this is not needed until the *FILESYS* assignment.
///
/// * `file` is the place to read the bitmap from.
void
Bitmap::FetchFrom(OpenFile *file)
{
    ASSERT(file != nullptr);
    file->ReadAt((char *) map, numWords * sizeof (unsigned), 0);
}

/// Store the contents of a bitmap to a Nachos file.
///
/// Note: this is not needed until the *FILESYS* assignment.
///
/// * `file` is the place to write the bitmap to.
void
Bitmap::WriteBack(OpenFile *file) const
{
    ASSERT(file != nullptr);
    file->WriteAt((char *) map, numWords * sizeof (unsigned), 0);
}



//////

Syncmap::Syncmap(unsigned nitems)
{
    ASSERT(nitems > 0);
    bmap = new Bitmap(nitems);
    lock = new Lock("Syncmap");
    record = new List <filedata *>;
}

/// De-allocate a bitmap.
Syncmap::~Syncmap()
{
    delete bmap;
    delete lock;
    delete record;
}

/// Set the “nth” bit in a bitmap.
///
/// * `which` is the number of the bit to be set.

void update(filedata* file, int r , int w){
    file->readers += r;
    file->writers += w;
    if(!file->uthreads->HasKey(currentThread->pid)){ //Si el thread no lo estaba usando lo agrego a la lista
        DEBUG('f', "Agrego el thread a la lista del archivo .\n");
        file->uthreads->SortedInsert(currentThread , currentThread->pid);
        //currentThread->active_reads += r;
        //currentThread->active_writes += w;
    }
}


void clear_table(filedata* file, int r , int w){
    file->readers -= r;
    file->writers -= w;
    if(file->uthreads->HasKey(currentThread->pid)){ //Si el thread esta en la lista, que deberia ser siempre si esta bien usado
        //file->uthreads->SortedInsert(currentThread , currentThread->pid);
        //currentThread->active_reads -= r;
        //currentThread->active_writes -= w;
        if(currentThread->active_reads == 0 && currentThread->active_writes == 0){ //si ya no tiene mas lectores o escritores vivos
            DEBUG('F', "Borro el thread %s de la lista del archivo abierto.\n",currentThread->GetName()); 
            ASSERT(currentThread == file->uthreads->KeyPop(currentThread->pid));
        }
        else{
            //file->uthreads->PrintL();
            DEBUG('F',"no borro nada porque %s tiene %d escritores y %d lectores.\n",currentThread->GetName(),currentThread->active_writes,currentThread->active_reads); 
        }
    }
}

void deletion(filedata *file,int r, int w){
    file->deleted = true;
}


void 
Syncmap::MarkDelete(unsigned sector){
    lock->Acquire();
    record->HasModify(sector,0,0,deletion);
    lock->Release();

}

void//0 si no usa el write o read 1 si lo usa
Syncmap::Mark(unsigned sector, int reader, int writer )
{
    ASSERT(sector < bmap->numBits);
    lock->Acquire();

        //si el sector esta activo, agrego el thread a la lista (si es que no esta) y agrego
        //despues sumo los lectores o escritores
        if(bmap->Test(sector)){//si esta en uso
            record->HasModify(sector,reader,writer,update);
            
        }
        else{ //si no esta en uso el archivo creo la entrada
            DEBUG('F', "Thread %s agrega nodo de archivo abierto (sector %d).\n",currentThread->GetName(),sector);
            filedata* data = (filedata*)malloc(sizeof(filedata));
            data->readers = reader;
            data->writers = writer;
            data->deleted = false;
            data->uthreads = new List <Thread *>;
            data->lock = new Lock("disklock");
            data->read = new Condition("Readcond",data->lock);
            data->write = new Condition("Writecond",data->lock);
            //data->lock->Acquire(); //lo adquiero 
            //currentThread->active_reads += reader;
            //currentThread->active_writes += writer;
            data->uthreads->SortedInsert(currentThread , currentThread->pid);
            record->SortedInsert(data,sector);
            bmap->Mark(sector);

        }
    lock->Release();
}

/// Clear the “nth” bit in a bitmap.
///
/// * `which` is the number of the bit to be cleared.

void
Syncmap::Clear(unsigned sector, int readers, int writers)
{
    ASSERT(sector < bmap->numBits);
    lock->Acquire();
    if(bmap->Test(sector)){//si esta en uso
        DEBUG('F', "Thread quiere cerrar (sector %d).\n",sector);
        
        record->HasModify(sector,readers,writers,clear_table);
        //PrintAll();
    }
    if(record->KeyCheck(sector)->uthreads->IsEmpty()){//si ya no tiene mas threads con el archivo abierto
        DEBUG('F', "Thread %s apaga el bit interno de sector %d.\n",currentThread->GetName(),sector);
        bmap->Clear(sector);
    }

    lock->Release();
}

/// Return true if the “nth” bit is set.
///
/// * `which` is the number of the bit to be tested.
bool
Syncmap::Test(unsigned which) const
{
    ASSERT(which < bmap->numBits);
    bool t = bmap->Test(which);
    return t;
}

/// Return the number of clear bits in the bitmap.  (In other words, how many
/// bits are unallocated?)
unsigned
Syncmap::CountClear() const
{
    unsigned count = 0;

    for (unsigned i = 0; i < bmap->numBits; i++)
        if (!Test(i))
            count++;
    return count;
}

/// Print the contents of the bitmap, for debugging.
///
/// Could be done in a number of ways, but we just print the indexes of all
/// the bits that are set in the bitmap.
void
Syncmap::Print() const
{
    printf("Syncmap bits set:\n");
    for (unsigned i = 0; i < bmap->numBits; i++)
        if (Test(i)){
            printf("%u ", i);
            
        }
    printf("\n");
}


filedata* Syncmap::FileCheck(unsigned sector){
    return record->KeyCheck(sector);
}

void
Syncmap::PrintAll() const
{

    for (unsigned i = 0; i < bmap->numBits; i++)
        if (Test(i)){
            printf("Sector %u \n", i);
            filedata * file = record->KeyCheck(i);
            file->uthreads->PrintL();
            printf("Lectores %d  ",file->readers);
            printf("Escritores %d \n",file->writers);
        }
    printf("\n");
}

