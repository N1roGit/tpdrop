/// Routines for managing the disk file header (in UNIX, this would be called
/// the i-node).
///
/// The file header is used to locate where on disk the file's data is
/// stored.  We implement this as a fixed size table of pointers -- each
/// entry in the table points to the disk sector containing that portion of
/// the file data (in other words, there are no indirect or doubly indirect
/// blocks). The table size is chosen so that the file header will be just
/// big enough to fit in one disk sector,
///
/// Unlike in a real system, we do not keep track of file permissions,
/// ownership, last modification date, etc., in the file header.
///
/// A file header can be initialized in two ways:
///
/// * for a new file, by modifying the in-memory data structure to point to
///   the newly allocated data blocks;
/// * for a file already on disk, by reading the file header from disk.
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2016-2020 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "file_header.hh"
#include "threads/system.hh"

#include <ctype.h>
#include <stdio.h>


/// Initialize a fresh file header for a newly created file.  Allocate data
/// blocks for the file out of the map of free disk blocks.  Return false if
/// there are not enough free blocks to accomodate the new file.
///
/// * `freeMap` is the bit map of free disk sectors.
/// * `fileSize` is the bit map of free disk sectors.

/*
bool
FileHeader::Allocate(Bitmap *freeMap, unsigned fileSize, int type = -1)
{
    ASSERT(freeMap != nullptr);

    if (fileSize > MAX_FILE_SIZE)
        return false;

    raw.numSectors = DivRoundUp(fileSize, SECTOR_SIZE);
    raw.type = type;

    if(type == -1){ //caso fileheader 'original'
        if (freeMap->CountClear() < raw.numSectors) return false;  // Not enough space.
        
        //raw.numBytes = fileSize;
        DEBUG('f', "Allocando %u bytes... ", fileSize);
        
        if(raw.numSectors >= NUM_DIRECT){
        DEBUG('f', "Necesito mas espacio para %dbytes... ", fileSize);
        int loaded = 0;
        for (unsigned i = 0; i < NUM_DIRECT; i++){
            int x = freeMap->Find();
            raw.dataSectors[x] = freeMap->Find();
            loaded++;
        }
        unsigned remaining = fileSize - loaded*SECTOR_SIZE;
        raw.next = (int*)(new FileHeader);
        ((FileHeader*)raw.next)->Allocate(freeMap,remaining,0);
        }

        for (unsigned i = 0; i < raw.numSectors; i++){
            int x = freeMap->Find();
            raw.dataSectors[i] = x;
        }
        return true;
    }
    if(type == 0){ //primera indireccion

        if (freeMap->CountClear() < raw.numSectors) return false;  // Not enough space.
            int index = DivRoundUp(raw.numSectors, NUM_DIRECT*NUM_DIRECT);
            int max_load = NUM_DIRECT * NUM_DIRECT * SECTOR_SIZE;
            DEBUG('f', "Creando %d indirecciones triples",index);
            for (unsigned i = 0; i < index; i++){
                raw.dataSectors[i] = (unsigned)(new FileHeader);

            loaded = min(max_load,fileSize);
            
            ASSERT(((FileHeader*)(raw.dataSectors[i]))->Allocate(freeMap,loaded,1));
            
            fileSize -= loaded;
            DEBUG('f', "Cargo %d bytes en la indireccion quedan %d",loaded,fileSize);
        }
        return true;

    }
    if(type == 1){ //este fileheader tiene solo indirecciones de segundo nivel
        if (freeMap->CountClear() < raw.numSectors) return false;  // Not enough space.
        //Index me va a aclarar cuantos arreglos tengo que definir
        int index = DivRoundUp(raw.numSectors, NUM_DIRECT);
        DEBUG('f', "Creando %d indirecciones triples",index);
        int loaded = 0;
        int max_load = NUM_DIRECT * SECTOR_SIZE;
        for (unsigned i = 0; i < index; i++){
            raw.dataSectors[i] = new FileHeader;

            loaded = min(max_load,fileSize);
            
            ASSERT(raw.dataSectors[i]->Allocate(freeMap,loaded,2));
            
            fileSize -= loaded;
            DEBUG('f', "Cargo %d bytes en la indireccion quedan %d",loaded,fileSize);
        }
        return true;
    }
    if(type == 2){
        if (freeMap->CountClear() < raw.numSectors) return false;
        for (unsigned i = 0; i < raw.numSectors; i++){
            raw.dataSectors[i] = freeMap->Find();
        }
        return true;
    }    
    ASSERT(false);
}
**/

unsigned minimo(unsigned a,unsigned b){
    if(a<b) return a;
    else return b;
}


FileHeader::FileHeader(int sctr) {
    //FileHeader* ret = new FileHeader;
    sector = sctr;
    //Leo los datos del sector, (asi sirve para buscar)
    synchDisk->ReadSector(sector, (char *)&raw);
    //return ret;
    DEBUG('f', "indice %d , next %d , numbytes %d , %d \n", raw.index,raw.next,raw.numBytes,sizeof(raw));
}


bool
FileHeader::Allocate(Bitmap *freeMap, unsigned fileSize){
    DEBUG('1', "Alocando %u bytes \n", fileSize);
    ASSERT(freeMap);

    raw.numBytes = fileSize;
    raw.numSectors = DivRoundUp(fileSize, SECTOR_SIZE);
    if (!fileSize) {
        DEBUG('f', "Se creo un archivo vacio.\n");
        return true;
    }
    if(!raw.numSectors){
        ASSERT(false);
    }

    if (freeMap->CountClear() < minimo(raw.numSectors,NUM_DIRECT-1)) {
        DEBUG('f', "ERROR me faltan: %u sectores.\n",freeMap->CountClear() );
        return false;
    }
    /* Caso Expand */
    unsigned i = 0;
    while (i != NUM_DIRECT && raw.dataSectors[i]!=0) i++;
    /* Caso Expand */
    for (; i < raw.numSectors; i++) {
        // Si me paso del arreglo del header, Aisgno un sector a una 'cabecera de extencion'
        if (i == NUM_DIRECT) {
            raw.next = freeMap->Find();
            if (raw.next == -1) 
                return false;
            DEBUG('f', "Me quede sin espacio, hago mas lugar en sector %d\n", raw.next);

            FileHeader* next = new FileHeader(raw.next);
            //seteo todo en 0 por si quedaba basura en el sector
            for (int z = 0; z < NUM_DIRECT; z++) 
                next->raw.dataSectors[z] = 0;

            next->raw.next = next->raw.numBytes = next->raw.numSectors = 0, next->raw.index = 0;
            
            next->raw.index = raw.index + 1 ;
            DEBUG('1', "next tiene indice %d y esta alocado en el sector %d \n",next->raw.index,raw.next);
            bool ret = next->Allocate(freeMap, raw.numBytes - MAX_FILE_SIZE);

            raw.numBytes = fileSize;
            raw.numSectors = NUM_DIRECT;
            next->WriteBack(raw.next);

            //delete next;
            return ret;
        }
        else
            raw.dataSectors[i] = freeMap->Find();
    }

    DEBUG('f', "Archivo creado.\n");

    return true;
}

/// De-allocate all the space allocated for data blocks for this file.
///
/// * `freeMap` is the bit map of free disk sectors.
void
FileHeader::Deallocate(Bitmap *freeMap)
{
        ASSERT(freeMap != nullptr);
    //limpio los sectores de este nodo
    for (unsigned i = 0; i < NUM_DIRECT; i++)
        if (freeMap->Test(raw.dataSectors[i]) && raw.dataSectors[i])
            freeMap->Clear(raw.dataSectors[i]);
    //limpieza recursiva
    if (raw.next)
    {  
        FileHeader* node = new FileHeader(raw.next);

        node->Deallocate(freeMap);

        freeMap->Clear(raw.next);
        delete node;
    }
}

/// Fetch contents of file header from disk.
///
/// * `sector` is the disk sector containing the file header.
void
FileHeader::FetchFrom(unsigned sectr)
{
    synchDisk->ReadSector(sectr, (char *) &raw);
}

/// Write the modified contents of the file header back to disk.
///
/// * `sector` is the disk sector to contain the file header.
void
FileHeader::WriteBack(unsigned sectr)
{
    synchDisk->WriteSector(sectr, (char *) &raw);
     DEBUG('2', "WRITE SECTOR %d indice %d , next %d , numbytes %d , %d \n", sectr,raw.index,raw.next,raw.numBytes,sizeof(raw));
    
}

/// Return which disk sector is storing a particular byte within the file.
/// This is essentially a translation from a virtual address (the offset in
/// the file) to a physical address (the sector where the data at the offset
/// is stored).
///
/// * `offset` is the location within the file of the byte in question.
unsigned
FileHeader::ByteToSector(unsigned offset) {
    unsigned i = (offset / SECTOR_SIZE) % NUM_DIRECT;

    if (offset / (NUM_DIRECT * SECTOR_SIZE) != raw.index) {
        DEBUG('f', "EL NEXT ESTA EN EL SECTOR %d ------------ i %d, offset %d , if %d != %d .\n",raw.next, i , offset, offset / (NUM_DIRECT * SECTOR_SIZE) , raw.index);
        ASSERT(raw.next);

        FileHeader* node =  new FileHeader(raw.next);
        DEBUG('f', "cargo el proximo nodo del sector %d \n",raw.next);
        unsigned ret = node->ByteToSector(offset);
        delete node;
        return ret;
    } else {
        //DEBUG('f', "encontre la cabecera que necesitaba\n");
        return raw.dataSectors[i];
    }
}

/// Return the number of bytes in the file.
unsigned
FileHeader::FileLength() const
{
   /* static int counter = 0;
    if(raw.next != 0)
    {
        FileHeader* f =  new FileHeader(raw.next);
        counter += raw.numBytes + f->FileLength();
        DEBUG('1', "SUMANDO TOTAL %d.\n",counter);

    }
    
    return counter;*/
    return raw.numBytes;
}

/// Print the contents of the file header, and the contents of all the data
/// blocks pointed to by the file header.
void
FileHeader::Print(const char *title)
{
    char *data = new char [SECTOR_SIZE];

    if (title == nullptr)
        printf("File header:\n");
    else
        printf("%s file header:\n", title);

    printf("    size: %u bytes\n"
           "    block indexes: ",
           raw.numBytes);
     
     
    int loop = 0;
    for(FileHeader * f = new FileHeader(sector) ; loop!=2 ; loop *= 2){
        for (unsigned i = 0; i < minimo(f->raw.numSectors,NUM_DIRECT); i++)
            printf("%u ", f->raw.dataSectors[i]);

        FileHeader * _f = f;
        //carga el proximo pedazo
        if(f->raw.next!=0){
        f = new FileHeader(f->raw.next);
        delete _f;
        }
        else loop = 1;
    }

    loop = 0;
    printf("\n");

    /*for (unsigned i = 0, k = 0; i < raw.numSectors; i++) {
        printf("    contents of block %u:\n", raw.dataSectors[i]);
        synchDisk->ReadSector(raw.dataSectors[i], data);
        for (unsigned j = 0; j < SECTOR_SIZE && k < raw.numBytes; j++, k++) {
            if (isprint(data[j]))
                printf("%c", data[j]);
            else
                printf("\\%X", (unsigned char) data[j]);
        }
        printf("\n");
    }*/
    unsigned k = 0;

    for(FileHeader * f = new FileHeader(sector) ; loop!=2 ; loop *= 2){
        printf("Proxima cabecera en bloque  %d\n",f->raw.next);
        for (unsigned i = 0; i < minimo(f->raw.numSectors,NUM_DIRECT); i++) {
            printf("    contents of block %u:\n", f->raw.dataSectors[i]);
            synchDisk->ReadSector(f->raw.dataSectors[i], data);

            for (unsigned j = 0; j < SECTOR_SIZE; j++) {  //&& k < minimo(f->raw.numBytes,NUM_DIRECT*SECTOR_SIZE)
                if (isprint(data[j]))
                    printf("%c", data[j]);
                else
                    printf("\\%X", (unsigned char) data[j]);
            }
            printf("\n");
        }
        //printf("bit ocupado por cabecera %d\n",f->raw.next);
        FileHeader * _f = f;
        //carga el proximo pedazo
        if(f->raw.next!=0){
        f = new FileHeader(f->raw.next);
        delete _f;
        }
        else loop = 1;
    }
    delete [] data;
}

const RawFileHeader *
FileHeader::GetRaw() const
{
    return &raw;
}


void
FileHeader::Expand(unsigned numBytes) {
    if(numBytes==0){
        DEBUG('f',"ESTAS EXPANDIENDO POR 0");
        ASSERT(false);
    };

    //actualizo los bytes
    raw.numBytes += numBytes;
    raw.numSectors = DivRoundUp(raw.numBytes, SECTOR_SIZE);
    WriteBack(sector); //aca lee otro sector  -Fixed
    //salto a la ultima cabecera
    //printf("B-------------->   %d    <----------------B\n",sector);

    if (raw.next) {
        FileHeader* node = new FileHeader(raw.next);
        node->Expand(numBytes);
        DEBUG('F', "new numbytes %d index %d \n",node->raw.numBytes,node->raw.index);
        delete node;
        return;
    }
    unsigned i = 0;
    while (i != NUM_DIRECT && raw.dataSectors[i]) i++;
    // y al ultimo sector no asignado de la ultima cabecera
    if (i < raw.numSectors) {
        DEBUG('F', "Necesito mas lugar, abriendo el freemap -- bytes %d \n",raw.numBytes);
        //abro freemap
        OpenFile* ffile = new OpenFile(0);
        Bitmap* freeMap = new Bitmap(NUM_SECTORS);
        freeMap->FetchFrom(ffile);
        Allocate(freeMap, raw.numBytes);
        WriteBack(sector);
        freeMap->WriteBack(ffile);
    }
}
