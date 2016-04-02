#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "runtime.h"

static uint8_t* tospace_begin;
static uint8_t* tospace_end;
static int initialized = 0;

// Object Tag (64 bits)
// See the object tag layout in passes/utils.rkt
static const int TAG_IS_NOT_FORWARD_MASK = 0b1;
static const int TAG_LENGTH_MASK = 0b1111110;
static const int64_t TAG_PTR_BITFIELD_MASK = 0x3ffffffffffff; // mask 50 bits
static const int TAG_PTR_BITFIELD_RSHIFT = 7;

// Check to see if a tag is actually a forwarding pointer.
static inline int is_forwarding(int64_t tag)
{
    return !(tag & TAG_IS_NOT_FORWARD_MASK);
}

// Get the length field out of a tag.
static inline int get_length(int64_t tag)
{
    return (tag & TAG_LENGTH_MASK) >> 1;
}

// Get the "is pointer bitfield" out of a tag.
static inline int64_t get_bitfield(int64_t tag)
{
    return (tag >> TAG_PTR_BITFIELD_RSHIFT) & TAG_PTR_BITFIELD_MASK;
}

// initialize the state of the collector so that allocations can occur
void initialize(uint64_t rootstack_size, uint64_t heap_size)
{
    // printf("initializing runtime with rootstack size %" PRIi64 ", heap size %" PRIi64 "\n",
    //        rootstack_size, heap_size);

    // 1. Check to make sure that our assumptions about the world are correct.
#ifndef NDEBUG
    if (sizeof(int64_t) != sizeof(int64_t*))
    {
        printf("The runtime was compiler on an incompatible platform.");
        exit(EXIT_FAILURE);
    }

    if ((heap_size % 8) != 0)
    {
        printf("Invalid heap size: %" PRIu64 ", heap must be 8-byte aligned.\n", heap_size);
        exit(EXIT_FAILURE);
    }

    if ((rootstack_size % 8) != 0)
    {
        // TODO (osa): Why?
        printf("Invalid rootstack size %" PRIu64 ", root stack must be 8-byte aligned.\n",
                rootstack_size);
        exit(EXIT_FAILURE);
    }
#endif

  // 2. Allocate memory (You should always check if malloc gave you memory)
  if (!(fromspace_begin = malloc(heap_size))) {
      printf("Failed to malloc %" PRIu64 " byte fromspace.\n", heap_size);
      exit(EXIT_FAILURE);
  }

  if (!(tospace_begin = malloc(heap_size))) {
      printf("Failed to malloc %" PRIu64 " byte tospace.\n", heap_size);
      exit(EXIT_FAILURE);
  }

  if (!(rootstack_begin = malloc(rootstack_size))) {
    printf("Failed to malloc %" PRIu64 " byte rootstack.", rootstack_size);
    exit(EXIT_FAILURE);
  }

  rootstack_ptr = rootstack_begin;

  // 2.5 Calculate the ends memory we are using.
  // Note: the pointers are for a half open interval [begin, end)
  fromspace_end = fromspace_begin + heap_size;
  tospace_end = tospace_begin + heap_size;
  rootstack_end = rootstack_begin + rootstack_size;

  // 3 Initialize the global free pointer
  free_ptr = fromspace_begin;

  // Useful for debugging
  initialized = 1;
}

int shutdown()
{
#ifndef NDEBUG
    if (rootstack_ptr != rootstack_begin)
    {
        printf("rootstack_ptr is not reset before shutdown.\n");
        printf("rootstack_ptr: %p rootstack_begin: %p\n",
               (void*)rootstack_ptr, (void*)rootstack_begin);
        return 1;
    }

#endif
    free(fromspace_begin);
    free(tospace_begin);
    free(rootstack_begin);
    return 0;
}

// cheney implements cheney's copying collection algorithm
// There is a stub and explaination below.
static void cheney();

void print_vector(int64_t* vector)
{
    int64_t info = *vector;

    if (is_forwarding(info))
    {
        printf("<vector: forwarding to: %p>\n", (void*)info);

        if ((void*)info >= (void*)tospace_begin &&
                (void*)info < (void*)tospace_end)
            printf("(%p is in tospace)\n", (void*)info);
        else if ((void*)info >= (void*)fromspace_begin &&
                (void*)info < (void*)fromspace_end)
            printf("(%p is in fromspace)\n", (void*)info);
        else
            printf("(%p is not in fromspace or tospace)\n", (void*)info);

        if (info)
        {
            printf("%p: ", (void*)info);
            print_vector((int64_t*)info);
        }
    }
    else
    {
        int fields = get_length(info);
        int64_t bitfield = get_bitfield(info);

        printf("<vector (%p, ", (void*) vector);
        if ((void*)vector >= (void*)tospace_begin &&
                (void*)vector < (void*)tospace_end)
            printf("in tospace):");
        else if ((void*)vector >= (void*)fromspace_begin &&
                (void*)vector < (void*)fromspace_end)
            printf("in fromspace):");
        else
            printf("\?\?\?):");

        for (int i = 0; i < fields; i++)
        {
            if ((bitfield & (1 << i)) != 0)
                printf(" ptr(%p)", (void*)(*(vector + 1 + i)));
            else
                printf(" %" PRIi64, *(vector + 1 + i));
        }
        printf(">\n");
    }
}

void print_root_stack()
{
    printf("[\n");

    int64_t** rootstack_work_ptr = (int64_t**)rootstack_begin;
    while ((void*)rootstack_work_ptr < (void*)rootstack_ptr)
    {
        print_vector(*rootstack_work_ptr);
        printf("---\n");
        rootstack_work_ptr++;
    }
    printf("]\n");
}

void print_pointers(void* free_ptr)
{
    printf("=== POINTERS ===\n");
    printf("fromspace begin = %p\n", (void*)fromspace_begin);
    printf("fromspace end   = %p\n", (void*)fromspace_end);
    printf("tospace   begin = %p\n", (void*)tospace_begin);
    printf("tospace   end   = %p\n", (void*)tospace_end);
    printf("free ptr        = %p\n", (void*)free_ptr);
    printf("rootstack ptr   = %p\n", (void*)rootstack_ptr);
    printf("================\n");
}

void collect(int64_t bytes_requested)
{
     // 1. Check our assumptions about the world
#ifndef NDEBUG
    if (!initialized)
    {
        printf("Collection tried with uninitialized runtime.\n");
        exit(EXIT_FAILURE);
    }

    if (rootstack_ptr < rootstack_begin)
    {
        printf("rootstack_ptr = %p < %p = rootstack_begin.\n",
                (void*)rootstack_ptr, (void*)rootstack_begin);
        exit(EXIT_FAILURE);
    }

    if (rootstack_ptr > rootstack_end)
    {
        printf("rootstack_ptr = %p > %p = rootstack_end.\n",
                (void*)rootstack_ptr, (void*)rootstack_end);
        exit(EXIT_FAILURE);
    }

    // This is fine! Rootstack can contain top-level closure pointers. We
    // should just ignore those pointers.
    //
    // for(int i = 0; rootstack_begin + i < rootstack_ptr; i += 8)
    // {
    //     uint8_t* a_root = rootstack_begin[i];
    //     if (!(fromspace_begin <= a_root && a_root <= fromspace_end - 8))
    //     {
    //         printf("rootstack contains non fromspace pointer\n");
    //         exit(EXIT_FAILURE);
    //     }
    // }

    if (bytes_requested < 0)
    {
        printf("Can't request negative bytes: %" PRIi64 "\n", bytes_requested);
        exit(EXIT_FAILURE);
    }
#endif

    // 2. Perform collection
    cheney();

    // 3. Check if collection freed enough space in order to allocate
    if (fromspace_end - free_ptr < bytes_requested)
    {
      /*
         If there is not enough room left for the bytes_requested,
         allocate larger tospace and fromspace.

         In order to determine the new size of the heap double the
         heap size until it is bigger than the occupied portion of
         the heap plus the bytes requested.

         This covers the corner case of heaps objects that are
         more than half the size of the heap. No a very likely
         scenario but slightly more robust.

         One corner case that isn't handled is if the heap is size
         zero. My thought is that malloc probably wouldn't give
         back a pointer if you asked for 0 bytes. Thus initialize
         would fail, but our runtime-config.rkt file has a contract
         on the heap_size parameter that the code generator uses
         to determine initial heap size to this is a non-issue
         in reality.
      */

      ptrdiff_t occupied_bytes = free_ptr - fromspace_begin;
      ptrdiff_t needed_bytes = occupied_bytes + bytes_requested;

      ptrdiff_t old_len = fromspace_end - fromspace_begin;
      ptrdiff_t new_len = old_len;

      while (new_len < needed_bytes)
          new_len *= 2;

      // Free and allocate a new tospace of size new_bytes
      free(tospace_begin);

      if (!(tospace_begin = malloc(new_len)))
      {
          printf("failed to malloc %ld byte fromspace", new_len);
          exit(EXIT_FAILURE);
      }

      tospace_end = tospace_begin + new_len;

      // The pointers on the stack and in the heap must be updated,
      // so this cannot be just a memcopy of the heap.
      // Performing cheney's algorithm again will have the correct
      // effect, and we have already implemented it.
      cheney();

      // Cheney flips tospace and fromspace. Thus, we allocate another
      // tospace not fromspace as we might expect.
      free(tospace_begin);

      if (!(tospace_begin = malloc(new_len)))
      {
          printf("failed to malloc %ld byte tospace", new_len);
          exit(EXIT_FAILURE);
      }

      tospace_end = tospace_begin + new_len;
    }
}

// copy_vector is responsible for doing a pointer oblivious
// move of vector data and updating the vector pointer with
// the new address of the data.
// There is a stub and explaination for copy_vector below.
static void copy_vector(int64_t** vector_ptr_loc);

int in_fromspace(void* ptr)
{
    return (ptr >= (void*)fromspace_begin && ptr < (void*)fromspace_end);
}

void cheney()
{
    free_ptr = tospace_begin;

    {
        // Step 1: Copy roots.
        int64_t** work_ptr = (int64_t**)rootstack_begin;

        while ((void*)work_ptr != (void*)rootstack_ptr)
        {
            if (in_fromspace((void*)(*work_ptr)))
                copy_vector(work_ptr);
            work_ptr++;
        }
    }

    {
        // Step 2: Scan copied roots.
        int64_t* work_ptr = (int64_t*)tospace_begin;

        while ((void*)work_ptr != (void*)free_ptr)
        {
            int64_t info = *work_ptr;
            int fields = get_length(info);
            int64_t bitfield = get_bitfield(info);

            for (int i = 0; i < fields; ++i)
            {
                if ((bitfield & (1 << i)) != 0)
                {
                    // found a pointer
                    int64_t** ptr = (int64_t**)(work_ptr + 1 + i);

                    if (in_fromspace((void*)(*ptr)))
                        copy_vector(ptr);
                }
            }

            work_ptr += 1 + fields;
        }
    }

    // Step 3: Swap to/from spaces
    void* tmp = tospace_begin;
    tospace_begin = fromspace_begin;
    fromspace_begin = tmp;

    tmp = tospace_end;
    tospace_end = fromspace_end;
    fromspace_end = tmp;
}

void copy_vector(int64_t** vector_ptr_loc)
{
    int64_t* vector = *vector_ptr_loc;
    int64_t info = *vector;

#ifndef NDEBUG
    if (info == 0)
    {
        printf("info is zero\n");
        exit(1);
    }
#endif

    if (is_forwarding(info))
    {
        *vector_ptr_loc = (int64_t*)info;
        return;
    }

    int fields = get_length(info);
    int len = 8 + (8 * fields);

    // OMG, first argument is DEST. So unlike AT&T syntax.
    memcpy(free_ptr, vector, len);

    *vector_ptr_loc = (int64_t*)free_ptr;
    // printf("vector copied: ");
    // print_vector(*vector_ptr_loc);

    free_ptr += len;
}


////////////////////////////////////////////////////////////////////////////////

int64_t read_int(int64_t closure)
{
    (void)closure;
    int64_t i;
    scanf("%" PRIi64, &i);
    return i;
}

int print_int(int64_t closure, int64_t x)
{
    (void)closure;
    printf("%" PRIi64, x);
    return 0;
}

int print_bool(int64_t closure, int64_t x)
{
    (void)closure;
    if (x)
    {
        printf("#t");
    }
    else
    {
        printf("#f");
    }
    return 0;
}

uint64_t project(int64_t* any_val, uint8_t* ty_ser)
{
    // length of top-level serialization
    uint8_t ser_len = *ty_ser;
    // length of serialization in the vec
    uint8_t ser_vec_len = (uint8_t)(*(any_val + 1));

    if (ser_len != ser_vec_len)
    {
        // TODO: fail with a helpful error message
        printf("project(): Types are not equal.\n");
        fflush(stdout);
        exit(EXIT_FAILURE);
    }

    if (memcmp((void*)(any_val + 1), (void*)(ty_ser + 1), (size_t)ser_len) != 1)
    {
        // TODO: fail with a helpful error message
        printf("project(): Types are not equal.\n");
        fflush(stdout);
        exit(EXIT_FAILURE);
    }

    // Type of Any is the type we expect, just read the field.
    int any_vec_len = get_length(*any_val);
    return *(any_val + any_vec_len);
}

int is_integer(uint64_t* any_val)
{
    uint64_t s = *(any_val + 2);
    return ((s & 0x0000000000000003) == 0);
}

int is_boolean(uint64_t* any_val)
{
    uint64_t s = *(any_val + 2);
    return ((s & 0x0000000000000003) == 1);
}

int is_vector(uint64_t* any_val)
{
    uint64_t s = *(any_val + 2);
    return ((s & 0x0000000000000003) == 2);
}

int is_procedure(uint64_t* any_val)
{
    uint64_t s = *(any_val + 2);
    return ((s & 0x0000000000000003) == 3);
}
