/// Routines to manage an open Nachos file.  As in UNIX, a file must be open
/// before we can read or write to it.  Once we are all done, we can close it
/// (in Nachos, by deleting the `OpenFile` data structure).
///
/// Also as in UNIX, for convenience, we keep the file header in memory while
/// the file is open.
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2016-2020 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "open_file.hh"
#include "file_header.hh"
#include "threads/system.hh"
//#include "threads/thread.hh"
#include "directory.hh"
#include "directory_entry.hh"


#include <string.h>
#include <stdlib.h>

/// Open a Nachos file for reading and writing.  Bring the file header into
/// memory while the file is open.
///
/// * `sector` is the location on disk of the file header for this file.
OpenFile::OpenFile(int sector)
{
    DEBUG('F', "Thread %s abre file sector %d.\n",currentThread->GetName(),sector);
    hdr = new FileHeader(sector);
    hdr->FetchFrom(sector);
    seekPosition = 0;

    sectr = sector;
    syncmap->Mark(sectr,0 , 0);
    //syncmap->PrintAll();
}

/// Close a Nachos file, de-allocating any in-memory data structures.
OpenFile::~OpenFile()
{   
    DEBUG('F', "Thread %s cierra file sector %d\n",currentThread->GetName(),sectr);
   // delete lock;
   // delete read;
   // delete write;
    syncmap->Clear(sectr,0,0); //se saca de la lista de threads con el file abierto
    delete hdr;
    syncmap->FileCheck(sectr)->lock->Acquire();
        //Si ya esta marcada como borrada la intento liberar
        if (syncmap->FileCheck(sectr)->deleted){
            Directory *dir = new Directory(10); //NUM_DIR_ENTRIES, para cantidad extensibles de archivos lo pongo en system.hh y no lo hardcodeo
            dir->FetchFrom(fileSystem->directoryFile);
            //dir->List();
            DEBUG('F', "Thread %s intenta borrar %s \n",currentThread->GetName(), dir->ReverseFind(sectr));
            fileSystem->Remove( dir->ReverseFind(sectr));
            delete dir;
            //delete c;
        }
    syncmap->FileCheck(sectr)->lock->Release();

}

/// Change the current location within the open file -- the point at which
/// the next `Read` or `Write` will start from.
///
/// * `position` is the location within the file for the next `Read`/`Write`.
void
OpenFile::Seek(unsigned position)
{
    seekPosition = position;
}

/// OpenFile::Read/Write
///
/// Read/write a portion of a file, starting from `seekPosition`.  Return the
/// number of bytes actually written or read, and as a side effect, increment
/// the current position within the file.
///
/// Implemented using the more primitive `ReadAt`/`WriteAt`.
///
/// * `into` is the buffer to contain the data to be read from disk.
/// * `from` is the buffer containing the data to be written to disk.
/// * `numBytes` is the number of bytes to transfer.

int
OpenFile::Read(char *into, unsigned numBytes)
{
    DEBUG('D', "%s Entro en read, cursor en %d.\n",currentThread->GetName(),seekPosition);
    ASSERT(into != nullptr);
    ASSERT(numBytes > 0);

    syncmap->FileCheck(sectr)->lock->Acquire();

    filedata* data = syncmap->FileCheck(sectr); //tira segfault aca, agregar el addd 0 0 al constructor
    DEBUG('f', "escritores = %d.\n",data->writers);
    while(syncmap->FileCheck(sectr)->writers != 0){
        syncmap->FileCheck(sectr)->write->Wait(); //espera a que no escriba nadie
    }//espero que nadie escriba
    DEBUG('D', "Preparando para leer %d bytes.\n",numBytes);
    syncmap->Mark(sectr,1 , 0);
    currentThread->active_reads++;
    int result = ReadAt(into, numBytes, seekPosition);
    syncmap->Mark(sectr,-1 , 0);
    currentThread->active_reads--;
    seekPosition += result;
    DEBUG('f', " %s Dejo de leer, ",currentThread->GetName());
    syncmap->FileCheck(sectr)->read->Signal(); //avisa que termino de leer

    //write->Signal(); //borrar 
    //DEBUG('f', "Aviso que termine de leer y ");

    syncmap->FileCheck(sectr)->lock->Release();
    DEBUG('f', "salgo de read con status %d.\n",result);
    DEBUG('D', "%s Salio de read, cursor en %d.\n",currentThread->GetName(),seekPosition);
    return result;
}

int
OpenFile::Write(const char *into, unsigned numBytes)
{
    DEBUG('D', "%s Entro en write, cursor en %d y %d escritories.\n",currentThread->GetName(),seekPosition ,syncmap->FileCheck(sectr)->writers);

    ASSERT(into != nullptr);
    ASSERT(numBytes > 0);

    //lock->Acquire();
    syncmap->FileCheck(sectr)->lock->Acquire();

    filedata* data = syncmap->FileCheck(sectr); //tira segfault aca, agregar el addd 0 0 al constructor
    DEBUG('f', "escritores = %d.\n",data->writers);

    while(syncmap->FileCheck(sectr)->writers != 0 || syncmap->FileCheck(sectr)->readers != 0 ){
        //write->Wait(); //espera a que no escriba nadie o que no lea nadie
        if(syncmap->FileCheck(sectr)->writers != 0 )syncmap->FileCheck(sectr)->write->Wait();
        if(syncmap->FileCheck(sectr)->readers != 0 )syncmap->FileCheck(sectr)->read->Wait();
        //if(data->readers != 0 ) read->Wait();
    }
    DEBUG('D', "Preparando para escribir %d bytes.\n",numBytes);
    syncmap->Mark(sectr,0 , 1);
    currentThread->active_writes++;
    int result = WriteAt(into, numBytes, seekPosition);
    seekPosition += result;
    currentThread->active_writes--;
    syncmap->Mark(sectr,0 , -1);
    DEBUG('f', "Dejo de escribir.\n");

    syncmap->FileCheck(sectr)->write->Signal();
    //DEBUG('f', "aviso que termine.\n");

    syncmap->FileCheck(sectr)->lock->Release();
        
    DEBUG('f', "salgo de write con status %d.\n",result);
    DEBUG('D', "%s Salio de write, escritores:  %d.\n",currentThread->GetName(),syncmap->FileCheck(sectr)->writers);

    return result;
}

/// OpenFile::ReadAt/WriteAt
///
/// Read/write a portion of a file, starting at `position`.  Return the
/// number of bytes actually written or read, but has no side effects (except
/// that `Write` modifies the file, of course).
///
/// There is no guarantee the request starts or ends on an even disk sector
/// boundary; however the disk only knows how to read/write a whole disk
/// sector at a time.  Thus:
///
/// For ReadAt:
///     We read in all of the full or partial sectors that are part of the
///     request, but we only copy the part we are interested in.
/// For WriteAt:
///     We must first read in any sectors that will be partially written, so
///     that we do not overwrite the unmodified portion.  We then copy in the
///     data that will be modified, and write back all the full or partial
///     sectors that are part of the request.
///
/// * `into` is the buffer to contain the data to be read from disk.
/// * `from` is the buffer containing the data to be written to disk.
/// * `numBytes` is the number of bytes to transfer.
/// * `position` is the offset within the file of the first byte to be
///   read/written.

int
OpenFile::ReadAt(char *into, unsigned numBytes, unsigned position)
{
    

    ASSERT(into != nullptr);

    ASSERT(numBytes > 0);
    //DEBUG('f', "Llamo ReadAt.\n");
    unsigned fileLength = hdr->FileLength();
    unsigned firstSector, lastSector, numSectors;
    char *buf;

    if (position >= fileLength)
        return 0;  // Check request.
    if (position + numBytes > fileLength)
        numBytes = fileLength - position;
    DEBUG('f', "Reading %u bytes at %u, from file of length %u.\n",
          numBytes, position, fileLength);

    firstSector = DivRoundDown(position, SECTOR_SIZE);
    lastSector = DivRoundDown(position + numBytes - 1, SECTOR_SIZE);
    numSectors = 1 + lastSector - firstSector;

    // Read in all the full and partial sectors that we need.
    buf = new char [numSectors * SECTOR_SIZE];
    for (unsigned i = firstSector; i <= lastSector; i++)
        synchDisk->ReadSector(hdr->ByteToSector(i * SECTOR_SIZE),
                              &buf[(i - firstSector) * SECTOR_SIZE]);

    // Copy the part we want.
    memcpy(into, &buf[position - firstSector * SECTOR_SIZE], numBytes);
    DEBUG('f',"Thread %s read %d chars \n",currentThread->GetName(),numBytes);

    delete [] buf;
    return numBytes;


}

int
OpenFile::WriteAt(const char *from, unsigned numBytes, unsigned position)
{
    ASSERT(from != nullptr);
    ASSERT(numBytes > 0);
    //DEBUG('f', "Llamo WriteAt.\n");
    unsigned fileLength = hdr->FileLength();
    unsigned firstSector, lastSector, numSectors;
    bool firstAligned, lastAligned;
    char *buf;
    /*if (position >= fileLength){
        DEBUG('f', "position %d >= fileLength %d .\n",position,fileLength);
        return 0;
        }  // Check request.*/
    if (position + numBytes > fileLength){//si me fuera a pasar
        unsigned extra = position + numBytes - fileLength;
        hdr->Expand(extra);
        //printf("A-------------->   %d    <----------------A\n",hdr->sector);
        DEBUG('f', "Expandi %d bytes la file.\n",extra);
    }
    DEBUG('f', "Writing %u bytes at %u, from file of length %u.\n",
          numBytes, position, fileLength);

    firstSector = DivRoundDown(position, SECTOR_SIZE);
    lastSector  = DivRoundDown(position + numBytes - 1, SECTOR_SIZE);
    numSectors  = 1 + lastSector - firstSector;

    buf = new char [numSectors * SECTOR_SIZE];

    firstAligned = position == firstSector * SECTOR_SIZE;
    lastAligned  = position + numBytes == (lastSector + 1) * SECTOR_SIZE;

    // Read in first and last sector, if they are to be partially modified.
    if (!firstAligned)
        ReadAt(buf, SECTOR_SIZE, firstSector * SECTOR_SIZE);
    if (!lastAligned && (firstSector != lastSector || firstAligned))
        ReadAt(&buf[(lastSector - firstSector) * SECTOR_SIZE],
               SECTOR_SIZE, lastSector * SECTOR_SIZE);

    // Copy in the bytes we want to change.
    memcpy(&buf[position - firstSector * SECTOR_SIZE], from, numBytes);

    // Write modified sectors back.
    for (unsigned i = firstSector; i <= lastSector; i++)
        synchDisk->WriteSector(hdr->ByteToSector(i * SECTOR_SIZE),
                               &buf[(i - firstSector) * SECTOR_SIZE]);
    delete [] buf;
    return numBytes;
}

/// Return the number of bytes in the file.
unsigned
OpenFile::Length() const
{
    return hdr->FileLength();
}
