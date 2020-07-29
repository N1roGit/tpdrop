/// Routines to manage address spaces (memory for executing user programs).
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2016-2020 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "address_space.hh"
#include "executable.hh"
#include "threads/system.hh"
#include <stdio.h> 
#include <string.h>



uint32_t min (uint32_t a , uint32_t b){
  if(a<b) return a;
  else return b;
}

uint32_t unpage(uint32_t virtualAddr, TranslationEntry *pageTable){
  uint32_t npage = virtualAddr / PAGE_SIZE;
  uint32_t offset = virtualAddr % PAGE_SIZE;
  uint32_t page  = pageTable[npage].physicalPage;
  return page * PAGE_SIZE + offset;
}




AddressSpace::AddressSpace(OpenFile *executable_file, int spaceId)
{
    ASSERT(executable_file != nullptr);

    Executable exe(executable_file);
    ASSERT(exe.CheckMagic());
    file = (void*)(executable_file);

    unsigned size = exe.GetSize() + USER_STACK_SIZE;
      // We need to increase the size to leave room for the stack.
    numPages = DivRoundUp(size, PAGE_SIZE);
    size = numPages * PAGE_SIZE;
    // How big is address space?

    //inicializamos el swap.pid
    //spaceId = currentThread->pid;
    char swapFileName[16];
    snprintf(swapFileName, 16, "SWAP.%d", spaceId);
    fileSystem->Create(swapFileName, spaceId);
    swapFile = fileSystem->Open(swapFileName);


  #ifndef DEMAND /*No es por demanda*/

    ASSERT(numPages <= coremap->getbitmap()->CountClear());
      // Check we are not trying to run anything too big -- at least until we
      // have virtual memory.

    DEBUG('v', "Initializing address space, num pages %u, size %u\n",
          numPages, size);
    DEBUG('a', "Initializing address space, num pages %u, size %u\n",
          numPages, size);

    char *mainMemory = machine->GetMMU()->mainMemory;
    // First, set up the translation.

    pageTable = new TranslationEntry[numPages];
    unsigned int i = 0;
    for (; i < numPages; i++) {
        pageTable[i].virtualPage  = i;
          // For now, virtual page number = physical page number.
        pageTable[i].physicalPage = coremap->Find(i,currentThread);//bitMap->Find();
        pageTable[i].valid        = true;
        pageTable[i].use          = false;
        pageTable[i].dirty        = false;
        pageTable[i].readOnly     = false;
          // If the code segment was entirely on a separate page, we could
          // set its pages to be read-only.
          //lo pongo en 0 aca arriba ya que se cuales son
        unsigned pageIndex = pageTable[i].physicalPage;
		    memset(mainMemory + pageIndex * PAGE_SIZE, 0, PAGE_SIZE);


    }
    #endif
    #ifdef DEMAND

    char *mainMemory = machine->GetMMU()->mainMemory;
    // First, set up the translation.

    pageTable = new TranslationEntry[numPages];

    DEBUG('z', "Initializing  lazy address space, num pages %u, size %u\n", numPages, size);

    unsigned int i = 0;
    for (; i < numPages; i++) {
        pageTable[i].virtualPage  = i;
          // For now, virtual page number = physical page number.
        pageTable[i].physicalPage = -1; //todavia no se cargo 

        pageTable[i].valid        = false;
        pageTable[i].use          = false;
        pageTable[i].dirty        = false;
        pageTable[i].readOnly     = false;
          // If the code segment was entirely on a separate page, we could
          // set its pages to be read-only.
        if(i<NUM_PHYS_PAGES){ //sacar el i<tlbsize
          DEBUG('V', "Agrego %d al lru.\n", i);
          if(!machine->GetMMU()->lru->Has(i))machine->GetMMU()->lru->Append(i);
        }

    }

    #endif
    uint32_t codeSize = exe.GetCodeSize();
    codesz = codeSize;
    uint32_t initDataSize = exe.GetInitDataSize();
    datasz = initDataSize;
    #ifndef DEMAND
    // Zero out the entire address space, to zero the unitialized data
    // segment and the stack segment.
    //memset(mainMemory, 0, size);

    // Then, copy in the code and data segments into memory.

    if (codeSize > 0) {

        uint32_t virtualAddr = exe.GetCodeAddr();
        DEBUG('a', "Initializing code segment, at 0x%X, size %u\n",virtualAddr, codeSize);
        
        uint32_t remaining = codeSize;
        for(unsigned int y = 0; y<i;y++){
          
          uint32_t load = min(PAGE_SIZE,remaining);
          uint32_t pageIndex = pageTable[y].physicalPage;

          uint32_t virtualAddrOffset = unpage(virtualAddr , pageTable);  //virtualAddr + pageIndex * PAGE_SIZE;
          virtualAddr += load;
          exe.ReadCodeBlock(&mainMemory[virtualAddrOffset], load, codeSize-remaining);


          DEBUG('a', "OFFSET %X \n",codeSize-remaining);
          remaining = remaining - load;
          DEBUG('a', "Loading code segment data at 0x%X , %u bytes remaining\n",virtualAddrOffset, remaining);
          if (remaining == 0) break;
        }
      //exe.ReadCodeBlock(&mainMemory[virtualAddr], codeSize, 0); //

    }
    if (initDataSize > 0) {
        uint32_t remaining = initDataSize;
        uint32_t virtualAddr = exe.GetInitDataAddr();
        DEBUG('a', "Initializing init data, at 0x%X, size %u\n", virtualAddr, initDataSize);
        for(unsigned int y = 0; y<i;y++){

          uint32_t load = min(PAGE_SIZE,remaining);
          uint32_t pageIndex = pageTable[y].physicalPage;
          uint32_t virtualAddrOffset = unpage(virtualAddr , pageTable);  //virtualAddr + pageIndex * PAGE_SIZE;
          virtualAddr += load;
          exe.ReadDataBlock(&mainMemory[virtualAddrOffset], load, initDataSize-remaining); //initdatasize-remaining
          remaining = remaining - load;
          DEBUG('a', "Loading init data, %u bytes remaining\n", remaining);
          if (remaining == 0) break;
          
        }
        //exe.ReadDataBlock(&mainMemory[virtualAddr], initDataSize, 0);

    }
    #endif

}

/// Deallocate an address space.
///
/// Nothing for now!
AddressSpace::~AddressSpace()
{
  	for(unsigned i = 0; i < numPages; i++){
      if(pageTable[i].physicalPage != -1)
		    coremap -> Clear(pageTable[i].physicalPage);
    }
    delete [] pageTable;
     DEBUG('w', "BORRE %p\n",file);
    delete (Executable*)file;
}

/// Set the initial values for the user-level register set.
///
/// We write these directly into the “machine” registers, so that we can
/// immediately jump to user code.  Note that these will be saved/restored
/// into the `currentThread->userRegisters` when this thread is context
/// switched out.
void
AddressSpace::InitRegisters()
{
    for (unsigned i = 0; i < NUM_TOTAL_REGS; i++)
        machine->WriteRegister(i, 0);

    // Initial program counter -- must be location of `Start`.
    machine->WriteRegister(PC_REG, 0);

    // Need to also tell MIPS where next instruction is, because of branch
    // delay possibility.
    machine->WriteRegister(NEXT_PC_REG, 4);

    // Set the stack register to the end of the address space, where we
    // allocated the stack; but subtract off a bit, to make sure we do not
    // accidentally reference off the end!
    machine->WriteRegister(STACK_REG, numPages * PAGE_SIZE - 16);
    DEBUG('a', "Initializing stack register to %u\n",
          numPages * PAGE_SIZE - 16);
}

/// On a context switch, save any machine state, specific to this address
/// space, that needs saving.
///
/// For now, nothing!
void
AddressSpace::SaveState() //recorre tlb y pasa la info a pagetable
{
#ifdef USE_TLB

    for (unsigned i = 0; i < TLB_SIZE; i++) {
        if(machine->GetMMU()->tlb[i].valid)SaveTLB(i); //guardo la pagina del tlb actualizada al pagetable
    }
#endif

}

/// On a context switch, restore the machine state so that this address space
/// can run.
///
/// For now, tell the machine where to find the page table.
void
AddressSpace::RestoreState()
{
    #ifndef USE_TLB
    machine->GetMMU()->pageTable     = pageTable;
    machine->GetMMU()->pageTableSize = numPages;
    #else
    for(int i = 0; i<TLB_SIZE;i++){
      machine->GetMMU()->tlb[i].valid=false;
    }
  #endif
}

TranslationEntry *
AddressSpace::getTable()
{
  return pageTable;
}

void
AddressSpace::LoadPage(unsigned virtualPageNumber,int victim){
  // supongo addrs codigo<datos<stack
  //ver si es stack codigo o datos, si es stack con rellenar todo en 0s me basta
  
  if(coremap->getbitmap()->CountClear()==0){//si esta lleno
    DEBUG('w', "se lleno.\n");
    coremap->FindData(victim).pid->space->RemovePage(victim); //borro la victima
  }

  
  int i = coremap->Find(virtualPageNumber,currentThread); //bitMap->Find();
  DEBUG('w', "tengo la pagina fisica %d.\n",i);
    TranslationEntry* page = &currentThread->space->pageTable[virtualPageNumber];

    page->virtualPage  = virtualPageNumber;
    page->physicalPage = i;
    page->valid        = true;

  Executable* exe = new Executable((OpenFile*)file);

  uint32_t codeSize = exe->GetCodeSize();

  uint32_t initDataSize = exe->GetInitDataSize();

  uint32_t initVirtualAddr = exe->GetInitDataAddr();

  uint32_t virtualAddr = exe->GetCodeAddr();

  char *mainMemory = machine->GetMMU()->mainMemory;


  if (pageTable[virtualPageNumber].dirty) { //la pagina esta en swap
    DEBUG('w', "Cargando pagina %d en marco %d desde swap.\n", virtualPageNumber, i);
    swapFile->ReadAt(&(mainMemory[i * PAGE_SIZE]), PAGE_SIZE, virtualPageNumber * PAGE_SIZE);
  }
  /*
    else{ //lo cargo del archivo
    if(virtualPageNumber*PAGE_SIZE > (codeSize + initDataSize)){//STACK
          unsigned pageIndex = i;
          memset(mainMemory + pageIndex * PAGE_SIZE, 0, PAGE_SIZE);
    }
    else if(virtualPageNumber*PAGE_SIZE > codeSize){ //data
        //uint32_t initVirtualAddr = exe->GetInitDataAddr();
        DEBUG('z', "Initializing init data, at 0x%X, size %u\n", i*PAGE_SIZE , initDataSize); //unpage(virtualPageNumber*PAGE_SIZE,pageTable)
        //exe->ReadDataBlock(&mainMemory[i*PAGE_SIZE], PAGE_SIZE,(virtualPageNumber*PAGE_SIZE)); //(virtualPageNumber*PAGE_SIZE)-initVirtualAddr
        exe->ReadDataBlock(mainMemory,PAGE_SIZE,unpage(virtualAddress,pageTable));

    }
    else{
        //uint32_t virtualAddr = exe->GetCodeAddr();
        DEBUG('z', "Initializing code, at 0x%X, size %u\n", i*PAGE_SIZE , codeSize);
        //exe->ReadCodeBlock(&mainMemory[i*PAGE_SIZE], PAGE_SIZE,(virtualPageNumber*PAGE_SIZE));
        exe->ReadCodeBlock(mainMemory,PAGE_SIZE,unpage(virtualAddress,pageTable));
        //exe->ReadCodeBlock(&mainMemory[unpage(virtualPageNumber,pageTable)], PAGE_SIZE, 0);
    }

  }*/

/*

  if(virtualPageNumber*PAGE_SIZE < codeSize ){
    DEBUG('z', "Initializing code, at 0x%X, size %u\n", i*PAGE_SIZE , codeSize);

    exe->ReadCodeBlock(&mainMemory[i*PAGE_SIZE],PAGE_SIZE, virtualPageNumber*PAGE_SIZE);
    //exe->ReadCodeBlock(mainMemory,PAGE_SIZE,unpage(virtualAddress,pageTable));
    
  }
  else if(virtualPageNumber*PAGE_SIZE < initDataSize ){
    DEBUG('z', "Initializing data, at 0x%X, size %u\n", i*PAGE_SIZE , codeSize);
    //exe->ReadDataBlock(mainMemory,PAGE_SIZE,unpage(virtualAddress,pageTable));
    exe->ReadDataBlock(&mainMemory[i*PAGE_SIZE],PAGE_SIZE, virtualPageNumber*PAGE_SIZE);

  }
  else{
    unsigned pageIndex = i;
    memset(mainMemory + pageIndex * PAGE_SIZE, 0, PAGE_SIZE);

  }*/

  else{
    unsigned need2load = PAGE_SIZE;
    unsigned load;
    int offset;
    unsigned virtualAddress = virtualPageNumber*PAGE_SIZE;

    if(virtualAddress < virtualAddr+codeSize){ // La direccion empieza adentro del code, entre la addr inicial del code y la final
      load = min(need2load,  virtualAddr + codeSize - virtualAddress);
      offset = virtualAddress-virtualAddr; 
      if(offset<0)offset=0;
      DEBUG('z', "Initializing code, at 0x%X, offset: %d size %u\n", i*PAGE_SIZE,offset , load);

      exe->ReadCodeBlock(&mainMemory[i*PAGE_SIZE],load, offset);
      virtualAddress = virtualAddress + load;
      need2load = need2load - load;
      
    }
    if((virtualAddress < initVirtualAddr+initDataSize) && ((need2load*initDataSize)>0)){ //si sigo teniendo espacio en la pagina y hay datos que cargar
      load = min(need2load,initVirtualAddr+initDataSize - virtualAddress);
      offset = virtualAddress-initVirtualAddr;
      if(offset<0)offset=0;
      DEBUG('z', "Initializing data, at 0x%X,  offset: %d  size %u\n", i*PAGE_SIZE,offset , load);

      exe->ReadDataBlock(&mainMemory[i*PAGE_SIZE+PAGE_SIZE-need2load],load, offset);
      virtualAddress = virtualAddress + load;
      need2load = need2load - load;

    }
    if(need2load>0){ //lo que sobra lo lleno de 0s
      unsigned pageIndex = i;
      memset(mainMemory + (pageIndex * PAGE_SIZE + PAGE_SIZE-need2load), 0, need2load);

    }
  }
/*
 else{
    memset(&mainMemory[i*PAGE_SIZE], 0, PAGE_SIZE);
    exe->file->ReadAt(&mainMemory[i*PAGE_SIZE], PAGE_SIZE, exe->header.code.inFileAddr + virtualPageNumber * PAGE_SIZE);
 }*/

}



int
AddressSpace::FindVictim(){

  /*int victim = machine->GetMMU()->next2go;
  machine->GetMMU()->next2go = (machine->GetMMU()->next2go + 1)%TLB_SIZE;*/
  /*int count = machine->GetMMU()->LRU[0];
  int victim = 0;
  for(int i = 1 ; i<TLB_SIZE;i++){
    if(count>machine->GetMMU()->LRU[i]){
      count = machine->GetMMU()->LRU[i];
      victim = i;
    }
  }*/

  ASSERT(!machine->GetMMU()->lru->IsEmpty());

  int victim = machine->GetMMU()->lru->Pop();
  DEBUG('V', "La proxima victima es la numero %d.\n", victim);
  //int victim_ppage = pageTable[victim_vpage/PAGE_SIZE].physicalPage;


  return victim;
  //return SystemDep::Random() % NUM_PHYS_PAGES;
}



void
AddressSpace::RemovePage(unsigned physicalPage)
{
    coredata data = coremap->FindData(physicalPage);
    AddressSpace* addressSpace = data.pid->space;
    unsigned virtualPage = data.vpn;
   
    // Invalida la entrada en TLB y resetea el contador de accesos
    for (unsigned i = 0; i < TLB_SIZE; i++)
        if (machine->GetMMU()->tlb[i].physicalPage == physicalPage && machine->GetMMU()->tlb[i].valid) {
            SaveTLB(i);//lo paso a pagetable 
            machine->GetMMU()->tlb[i].physicalPage = -1;
            machine->GetMMU()->tlb[i].valid = false;
            machine->GetMMU()->tlb[i].dirty = false;
            machine->GetMMU()->tlb[i].use = false;
            machine->GetMMU()->tlb[i].readOnly = false;
            //machine->GetMMU()->LRU[i] = -1;
        }

    // Si la pagina esta sucia la guarda en disco
    if (addressSpace->pageTable[virtualPage].dirty)
        addressSpace->SwapPage(physicalPage, virtualPage);

    // Invalida la entrada en la pageTable
    addressSpace->pageTable[virtualPage].physicalPage = -1;
    addressSpace->pageTable[virtualPage].valid = false;


    coremap->Clear(physicalPage);
}

void
AddressSpace::SwapPage(int physicalPage, int virtualPage)
{
    DEBUG('w', "Escribiendo pagina virtual %d en el marco fisico %d del disco.\n", virtualPage, physicalPage);

    unsigned physicalAddress = physicalPage * PAGE_SIZE;
    char* mainMemory = machine->GetMMU()->mainMemory;
    swapFile->WriteAt(&mainMemory[physicalAddress], PAGE_SIZE, virtualPage * PAGE_SIZE);
}

void AddressSpace::Update(unsigned virtualAddress){
  
  unsigned virtualPage = virtualAddress / PAGE_SIZE;
  int victim = FindVictim();
  if (machine->GetMMU()->tlb[victim%TLB_SIZE].valid) SaveTLB(victim%TLB_SIZE); //guardo la pagina del tlb actualizada al pagetable
    // Si la pagina no esta la cargo en memoria
    if (pageTable[virtualPage].physicalPage == -1){
        DEBUG('v', "Cargando pagina virtual %d en la entrada %d de la TLB.\n", virtualPage, victim);
        LoadPage(virtualPage,victim); //GUARDar info de tlb victim %tlbsize y dumpear la info en la tabla de pagina en cada if bit.dirty
        machine->GetMMU()->tlb[victim % TLB_SIZE] = currentThread->space->pageTable[virtualPage];
    }
    
    else{
      DEBUG('v', "Copiando pagina virtual %d en la entrada %d de la TLB.\n", virtualPage, victim);
      //si no esta en la tlb pero ya esta cargada en la pagetable
      machine->GetMMU()->tlb[victim % TLB_SIZE] = pageTable[virtualPage];
      pageTable[virtualPage].valid = true;
    }
    LRU_Update(victim);
}

void AddressSpace::LRU_Update(int index){
    //machine->GetMMU()->LRU[index]++;
    //elimino si hay un duplicado
    if(machine->GetMMU()->lru->Has(index)){
      machine->GetMMU()->lru->Remove(index);
    }
    DEBUG('V', "Agrego %d al lru.\n", index);
    machine->GetMMU()->lru->Append(index);
    
}

void AddressSpace::SaveTLB(unsigned i){
  if(machine->GetMMU()->tlb[i].valid && pageTable){
    TranslationEntry save =  machine->GetMMU()->tlb[i];
    unsigned vpn = save.virtualPage;
    pageTable[vpn] = save;
}
}
