#ifndef STACK_C
#define STACK_C

#include <stdio.h>
#include "type.c"
#include <stdlib.h>
#include "gc.h"

#define malloc  GC_MALLOC // Use boehm to manage memory for us
#define realloc GC_REALLOC

#define INITIAL_LIST_SIZE 8
#define NEXT_LIST_SIZE 13
//#define LIST_GROWTH_RATIO 1.5

#ifdef DEBUG // Push an object to the stack. Print if debugging.
  #define push(object_type, object) \
    {puts("PUSHING: "#object); \
    push_object((cognate_object){.type=object_type, .object_type=object});}
#else
  #define push(object_type, object) \
    push_object((cognate_object){.type=object_type, .object_type=object})
#endif

#define pop(object_type) \
  check_type(object_type, pop_object()) . object_type

#define peek(object_type) \
  check_type(object_type, peek_object()) . object_type

static void init_stack(void);
static void push_object(cognate_object);
static cognate_object pop_object(void);
static cognate_object peek_object(void);
static void expand_stack();

cognate_list stack;
cognate_object* stack_end;

static void init_stack()
{
  //printf("ALLOCATING %i BYTES\n", INITIAL_LIST_SIZE * sizeof(cognate_object));
  // Allocate dynamic stack memory.
  stack.top = stack.start = (cognate_object*) malloc (INITIAL_LIST_SIZE * sizeof(cognate_object));
  stack_end = stack.start + INITIAL_LIST_SIZE;
}

static void push_object(cognate_object object)
{
  if (stack.top == stack_end)
    expand_stack();
  *stack.top++ = object;
}

static cognate_object pop_object(void)
{ 
  if (stack.top == stack.start) throw_error("Stack underflow!");
  return *--stack.top;
}

static cognate_object peek_object(void)
{
  if (stack.top == stack.start) throw_error("Stack underflow!");
  return *(stack.top - 1);
}

static void expand_stack()
{
  // Simple stack expansion with growth ratio.
  //expand_stack((stack.top - stack.start) * LIST_GROWTH_RATIO);
  
  // More advanced fibonacci stack expansion.
  // The ideal expansion ratio is the golden ratio.
  // The fibonacci sequence approaches the golden ratio.
  // It also frees blocks of memory exactly the right size to be allocated later.
  static size_t t1 = INITIAL_LIST_SIZE; // Current list size.
  static size_t t2 = NEXT_LIST_SIZE; // New list size.

  stack.start = (cognate_object*) realloc (stack.start, t2 * sizeof(cognate_object));
  stack.top = stack.start + t1;
  stack_end = stack.start + t2;

  t1 ^= t2;
  t2 ^= t1;
  t1 ^= t2;
  t2 += t1;
}

#endif
