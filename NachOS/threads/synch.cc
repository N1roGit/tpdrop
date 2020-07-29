/// Routines for synchronizing threads.
///
/// Three kinds of synchronization routines are defined here: semaphores,
/// locks and condition variables (the implementation of the last two are
/// left to the reader).
///
/// Any implementation of a synchronization routine needs some primitive
/// atomic operation.  We assume Nachos is running on a uniprocessor, and
/// thus atomicity can be provided by turning off interrupts.  While
/// interrupts are disabled, no context switch can occur, and thus the
/// current thread is guaranteed to hold the CPU throughout, until interrupts
/// are reenabled.
///
/// Because some of these routines might be called with interrupts already
/// disabled (`Semaphore::V` for one), instead of turning on interrupts at
/// the end of the atomic operation, we always simply re-set the interrupt
/// state back to its original value (whether that be disabled or enabled).
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2016-2020 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "synch.hh"
#include "system.hh"


/// Initialize a semaphore, so that it can be used for synchronization.
///
/// * `debugName` is an arbitrary name, useful for debugging.
/// * `initialValue` is the initial value of the semaphore.
Semaphore::Semaphore(const char *debugName, int initialValue)
{
    name  = debugName;
    value = initialValue;
    queue = new List<Thread *>;
}

/// De-allocate semaphore, when no longer needed.
///
/// Assume no one is still waiting on the semaphore!
Semaphore::~Semaphore()
{
    delete queue;
}

const char *
Semaphore::GetName() const
{
    return name;
}

/// Wait until semaphore `value > 0`, then decrement.
///
/// Checking the value and decrementing must be done atomically, so we need
/// to disable interrupts before checking the value.
///
/// Note that `Thread::Sleep` assumes that interrupts are disabled when it is
/// called.
void
Semaphore::P()
{
    DEBUG('s', "El semaforo %s hizo un P\n",this->GetName());
    IntStatus oldLevel = interrupt->SetLevel(INT_OFF);
      // Disable interrupts.

    while (value == 0) {  // Semaphore not available.
        queue->Append(currentThread);  // So go to sleep.
        currentThread->Sleep();
    }
    value--;  // Semaphore available, consume its value.

    interrupt->SetLevel(oldLevel);  // Re-enable interrupts.
}

/// Increment semaphore value, waking up a waiter if necessary.
///
/// As with `P`, this operation must be atomic, so we need to disable
/// interrupts.  `Scheduler::ReadyToRun` assumes that threads are disabled
/// when it is called.
void
Semaphore::V()
{
    DEBUG('s', "El semaforo %s hizo un V\n",this->GetName());
    IntStatus oldLevel = interrupt->SetLevel(INT_OFF);

    Thread *thread = queue->Pop();
    if (thread != nullptr)
        // Make thread ready, consuming the `V` immediately.
        scheduler->ReadyToRun(thread);
    value++;

    interrupt->SetLevel(oldLevel);
}

/// Dummy functions -- so we can compile our later assignments.
///
/// Note -- without a correct implementation of `Condition::Wait`, the test
/// case in the network assignment will not work!

Lock::Lock(const char *debugName)
{
    name = debugName;
    mutex = new Semaphore(name,1);
    name = debugName;
    owner = NULL;
   
}

Lock::~Lock()
{
    mutex->~Semaphore();
    
}

const char *
Lock::GetName() const
{
    return name;
}

void
Lock::Acquire()
{
    ASSERT(!IsHeldByCurrentThread());
    if(owner != nullptr){
        if(currentThread->get_priority() > owner->get_priority()){
            scheduler->Promote(owner, currentThread->get_priority());
        }
    }
    mutex->P();
    DEBUG('s', " %s entro en el lock %s \n",currentThread->GetName(),this->GetName());
    owner = currentThread;
}

void
Lock::Release()
{
    ASSERT(IsHeldByCurrentThread());
    owner = NULL;
    mutex->V();
    DEBUG('s', " %s salio del lock %s \n",currentThread->GetName(),this->GetName());

}

bool
Lock::IsHeldByCurrentThread() const
{
    return(owner == currentThread);
    //return false;
}

Condition::Condition(const char *debugName, Lock *conditionLock)
{
    name = debugName;
    conditionL = conditionLock;
    cola = new List <Semaphore*>;

}

Condition::~Condition()
{
    delete cola;
}

const char *
Condition::GetName() const
{
    return name;
}

void
Condition::Wait()
{
    DEBUG('c', "Thread %s esperando la condicion %s.\n", currentThread->GetName(), name);

    ASSERT(conditionL->IsHeldByCurrentThread());
    Semaphore *s = new Semaphore(name,0);
    cola->Append(s);
    conditionL->Release();
    s->P(); //aca se duerme 
    conditionL->Acquire();//aca ya le llego la señal y agarro el lock
    delete s;


}

void
Condition::Signal()
{
    DEBUG('c', "Thread %s haciendo una señal en %s.\n", currentThread->GetName(), name);
    ASSERT(conditionL->IsHeldByCurrentThread());
    
    
    //si hay gente esperando saco el primero y lo libero
    if(! cola->IsEmpty() ){
        cola->Pop()->V();
    }

}

void
Condition::Broadcast()
{
    ASSERT(conditionL->IsHeldByCurrentThread());
    DEBUG('c', "Thread %s haciendo broadcast en %s.\n", currentThread->GetName(), name);
    
    //Despierto a todos
    while(! cola->IsEmpty() ){
        cola->Pop()->V();
    }
}


Channel::Channel(const char* debugName){
    name = debugName;
    lock = new Lock(name);
    receptor = new Condition("receptor",lock);
    transmisor = new Condition("transmisor",lock);
    isEmpty =true;
    ack =  new Condition("llego el mensaje",lock); ;
}

Channel::~Channel(){
    delete lock;
    delete receptor;
    delete transmisor;
    delete ack;
}
void
Channel::Send(int msg){
    lock->Acquire();
        while (!isEmpty)
            transmisor->Wait();
        buffer = msg;
        isEmpty = false;
        receptor->Signal();
        ack->Wait();
    lock->Release();
}
void
Channel::Receive(int *msg){
    lock->Acquire();
    while(isEmpty){
        receptor->Wait();
    }
    (*msg) = buffer;
    isEmpty = true;
    ack->Signal();
    transmisor->Signal();
    lock->Release();
}