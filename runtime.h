#ifndef __RUNTIME_H
#define __RUNTIME_H

// At several points in our compiler we have decided to rely on the
// fact that pointers are 64 bits wide. The stdint.h header file
// declares a platform aware type that is guaranteed to contain
// 64-bits.
#include <stdint.h>

// Fromspace is our heap which is conceptually an array of 64 bit data
// unless meta information tells us more about about their contents.
//
// NOTE (osa): I don't understand what's special about 64 bit integers. The GC
// code is horrible in that first it uses sizeof(int64_t) and 8 inconsistently,
// second you need to remember to multiply your values with sizeo(int64_t) to
// make sure you do the byte calculation right. Here I'm fixing this. (these
// were int64_t* previously)
uint8_t* fromspace_begin;
uint8_t* fromspace_end;

// The free pointer should always point to the next free memory
// location. While the mutator (user program) is running this
// should always be pointing into fromspace.
uint8_t* free_ptr;

// The root stack is an array of pointers into the heap.  During calls
// to the collector only pointers between the roostack_ptr and
// rootstack_begin are considered as live roots.
uint8_t** rootstack_begin;
uint8_t** rootstack_ptr; // current top of the root stack
uint8_t** rootstack_end;

// Initialize the memory of the runtime with a fixed rootstack size
// and initial heap size.
void initialize(uint64_t rootstack_size, uint64_t heap_size);

// Shutdown the RTS. Free rootstack and heap.
int shutdown();

// Collect garbage data making room for a requested amount of memory.
// Use the pointers in the rootstack to determine what values in the
// heap are still live.
void collect(int64_t bytes_requested);

// Read an integer from stdin.
int64_t read_int(int64_t _closure);

// Print an integer to stdout.
int print_int(int64_t _closure, int64_t x);

// Print a boolean to stdout.
int print_bool(int64_t _closure, int64_t x);

#endif
