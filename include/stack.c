#ifndef STACK_C
#define STACK_C

#include <stdio.h>
#include "type.c"
#include <stdlib.h>
#include "gc.h"

#define malloc  GC_MALLOC // Use boehm to manage memory for us
#define realloc GC_REALLOC
// IF THERE ARE ANY STACK ERRORS, CHANGE malloc_atomic TO JUST malloc
#define malloc_atomic GC_MALLOC_ATOMIC

#define INITIAL_LIST_SIZE 16 // Constant values for initialising stack sizes.
#define LIST_GROWTH_FACTOR 1.5

#ifdef debug // Push an object to the stack. Print if debugging.
  #define push(object_type, object) \
    fprintf(stderr, "[DEBUG] %s:%d -> Pushing %s\n", __FILE__, __LINE__, #object); \
    push_any((cognate_object){.object_type=object, .type=object_type});
#else
  #define push(object_type, object) \
    push_any((cognate_object){.object_type=object, .type=object_type})
#endif

#define pop(object_type) \
  check_type(object_type, pop_any()) . object_type

#define peek(object_type) \
  check_type(object_type, peek_any()) . object_type

static void init_stack();
static void push_any(cognate_object);
static cognate_object pop_any();
static cognate_object peek_any();
static void expand_stack();

struct cognate_stack
{
  cognate_list    items;    // The list holding the stack itself.
  ptrdiff_t       size;     // Allocated size of the stack.
  cognate_object* modified; // Lowest stack element modified by current block.
};

typedef struct cognate_stack cognate_stack;

static cognate_stack stack;

static void init_stack()
{
  //printf("ALLOCATING %i BYTES\n", INITIAL_LIST_SIZE * sizeof(cognate_object));
  // Allocate dynamic stack memory.
  stack.modified = stack.items.top = stack.items.start = (cognate_object*) GC_MALLOC_ATOMIC ((stack.size = INITIAL_LIST_SIZE) * sizeof(cognate_object));
}

static void push_any(cognate_object object)
{
  // Profiles says that this function is The Problem.
  if (stack.items.start + stack.size == stack.items.top)
    expand_stack();
  *stack.items.top++ = object;
}

static cognate_object pop_any()
{ 
#ifndef unsafe
  if (stack.items.top == stack.items.start) 
    throw_error("Stack underflow!");
#endif
  stack.modified -= (stack.modified == stack.items.top);
  return *--stack.items.top;
}

static cognate_object peek_any()
{
  if (stack.items.top == stack.items.start) throw_error("Stack underflow!");
  return *(stack.items.top - 1);
}

static void expand_stack()
{
  // New stack size = current stack size * growth factor.

  #ifdef EBUG
    fprintf(stderr, "[DEBUG] %s:%d -> Expanding list/stack from length %lu to %lu\n", __FILE__, __LINE__, stack.size, (size_t)(stack.size * LIST_GROWTH_FACTOR)); 
  #endif
 
  int temp = stack.modified - stack.items.start;
  stack.items.start = (cognate_object*) realloc (stack.items.start, stack.size * LIST_GROWTH_FACTOR * sizeof(cognate_object));
  stack.modified = stack.items.start + temp;
  stack.items.top = stack.items.start + stack.size;
  stack.size *= LIST_GROWTH_FACTOR;
}

#endif
