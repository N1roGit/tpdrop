#include "lib/bitmap.hh"
#include "coremap.hh"
#include <stdlib.h>
Coremap::Coremap(unsigned nitems)
{
    bitmap = new Bitmap(nitems);
    max = nitems;
    //coredata datos[max];
    core = (coredata*)malloc(sizeof(coredata)*max);

    for(int i = 0 ; i < max ; i++){
        core[i].vpn = -1;
        core[i].pid = nullptr;
    }

}


Coremap::~Coremap(){
    delete bitmap;
    free(core);
}


int
Coremap::Find(int vpage, Thread* Pid){
    int i  = bitmap->Find();
    DEBUG('w', "llame al find, me dio %d ,quedan %d.\n",i,bitmap->CountClear() );
    if( i != -1){
        //DEBUG('v', " i: %d vpn: %d , pid: %d\n",i,vpage,Pid);
        core[i].vpn = vpage;
        core[i].pid = Pid;
    }

    return i;
}

void 
Coremap::Clear(unsigned which){
    if(which >= 0){
        bitmap->Clear(which);
        core[which].vpn = -1;
        core[which].pid = nullptr;
        DEBUG('w', "llame al clear, libero %d ,quedan %d.\n",which,bitmap->CountClear() );
    }
}


coredata 
Coremap::FindData(int ppage){
    coredata datonio;
    datonio.pid = core[ppage].pid;
    datonio.vpn = core[ppage].vpn;
    return datonio;
}


Bitmap* Coremap::getbitmap(){
    return bitmap;
}