
#ifndef COREMAP
#define COREMAP
//#include "lib/bitmap.hh"
#include "threads/thread.hh"

struct Bitmap;
typedef struct _coredata
{
    Thread* pid; //null si no esta en uso
    int vpn; // -1 si no esta en uso

} coredata ;


class Coremap {
public:

    /// Initialize a bitmap with `nitems` bits; all bits are cleared.
    ///
    /// * `nitems` is the number of items in the bitmap.
    Coremap(unsigned nitems);

    /// Uninitialize a bitmap.
    ~Coremap();

    //encuentra un bit vacio y lo activa, si falla tira -1. Ademas actualiza coremap
    int Find(int vpage, Thread* Pid);

    void Clear(unsigned which);

    coredata FindData(int ppage);

    Bitmap* getbitmap();

private:
    Bitmap* bitmap;
    unsigned max;
    //guardo los datos cada pagina fisica core[i], guardo (-1,npaginavirtual) si esta libre o guardo el pid si esta ocupada
    coredata* core;


};
#endif