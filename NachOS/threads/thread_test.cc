/// Simple test case for the threads assignment.
///
/// Create several threads, and have them context switch back and forth
/// between themselves by calling `Thread::Yield`, to illustrate the inner
/// workings of the thread system.
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2007-2009 Universidad de Las Palmas de Gran Canaria.
///               2016-2017 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "system.hh"
#include "synch.hh"
#include <stdio.h> 
#include <string.h>
#include <unistd.h>
#include <stdlib.h>


///#define SEMAPHORE_TEST true

#ifdef SEMAPHORE_TEST
Semaphore semaforo("Ej15",3); /// str para el debug, valor del semaforo
#endif

Lock lock("ej1");
/// Loop 10 times, yielding the CPU to another ready thread each iteration.
///
/// * `name` points to a string with a thread name, just for debugging
///   purposes.
int contador = 0;
void
SimpleThread(void *name_)
{

    #ifdef SEMAPHORE_TEST
    semaforo.P();
    #endif
    lock.Acquire();
    // Reinterpret arg `name` as a string.
    char *name = (char *) name_;


    // If the lines dealing with interrupts are commented, the code will
    // behave incorrectly, because printf execution may cause race
    // conditions.
    for (unsigned num = 0; num < 10; num++) {
        printf("*** Thread `%s` is running: iteration %u\n", name, num);
        contador ++;
        currentThread->Yield();
    }

    printf("!!! Thread `%s` has finished\n", name);
    printf("contador %d\n",contador);
    lock.Release();
    
    #ifdef SEMAPHORE_TEST

    semaforo.V(); 
    #endif
}

/// Set up a ping-pong between several threads.
///
/// Do it by launching ten threads which call `SimpleThread`, and finally
/// calling `SimpleThread` ourselves.
void
ThreadTest()
{
    DEBUG('t', "Entering thread test\n");

    ///char *name = new char [64];
    ///strncpy(name, "2nd", 64);
   /// Thread *newThread = new Thread(name);
   /// newThread->Fork(SimpleThread, (void *) name);
    for(int i = 0; i<4 ;i = i+1){
        char *name = new char [64];
        sprintf(name, "%c", 'B'  + i);
        Thread *newThread = new Thread(name,true,4);
        newThread->Fork(SimpleThread, (void *) name);
    }

    SimpleThread((void *) "A");
}

//Lock lock("lock");
unsigned int hamburgesas = 0;
Condition cond_lleno("cond_lleno",&lock);

Condition cond_vacio("cond_vacio",&lock);

void SimpleConsumer(void *name){
    while(22){
        //currentThread->Yield();
        sleep(rand()%2);
        lock.Acquire();
        cond_vacio.Wait();
        hamburgesas = hamburgesas-1;
        printf("%s se comio una hamburgesa quedan %d \n",name,hamburgesas);
        cond_lleno.Signal(); //avisa que no esta lleno
        lock.Release();
    }

}
void SimpleProducer(void *name){
    while(22){
        //currentThread->Yield();
        sleep(rand()%2);
        lock.Acquire();
        while(hamburgesas==2) cond_lleno.Wait();
        hamburgesas = hamburgesas+1;
        printf( "%s armo una hamburgesa, hay %d \n",name, hamburgesas);
        cond_vacio.Signal(); //avisa que no esta vacio
        lock.Release();
    }

}

void ConditionTest(){
    for(int i = 0; i<10 ;i = i+1){
        char *name = new char [64];
        sprintf(name, "%c", 'b'  + i);
        Thread *newThread = new Thread(name,true,4);
        newThread->Fork(SimpleConsumer, (void *) name);
    }
    for(int i = 0; i<4 ;i = i+1){
    char *name = new char [64];
        sprintf(name, "%c", 'B'  + i);
        Thread *newThread = new Thread(name,true,4);
        newThread->Fork(SimpleProducer, (void *) name);    
    }

   // SimpleProducer((void *) "A");
}

Channel ch("Canal");

void SimpleSender(void * name){
    ch.Send(rand()%100);
}

void SimpleReceiver(void* name){
    int i;
    ch.Receive(&i);
    printf("%s %d\n",name,i);
}

void funcion(void* n){
    sleep(5);
    printf("este es el hijo\n");
    }

void ChannelTest(){
    for(int i = 0; i<2 ;i = i+1){
    char *name = new char [64];
    sprintf(name, "%c", 'b'  + i);
    Thread *newThread = new Thread(name,true,4);
    newThread->Fork(SimpleSender, (void *) name);
    }

    for(int i = 0; i<2 ;i = i+1){
    char *name = new char [64];
        sprintf(name, "%c", 'B'  + i);
        Thread *newThread2 = new Thread(name,true,4);
        newThread2->Fork(SimpleReceiver, (void *) name);
    }
}

void JoinTest(){
    Thread *t= new Thread("Hijo",true,2);

    t->Fork(funcion,(void *)"0");
    t->Join();
    printf("este es el padre\n");

}