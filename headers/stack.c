#ifndef STACK_C
#define STACK_C

#include <stdio.h>
#include "type.c"
#include <stdlib.h>
#include "gc.h"

#define malloc  GC_MALLOC // Use boehm to manage memory for us
#define realloc GC_REALLOC

#define STACK_SIZE 8 // Constant values for initialising stack sizes.
#define STACK_SIZE_NEXT 13

#ifdef DEBUG // Push an object to the stack. Print if debugging.
  #define push(object_type, object) \
    {fprintf(stderr, "[DEBUG]%s:%d -> Pushing %s\n", __FILE__, __LINE__, #object); \
    push_object((cognate_object){.type=object_type, .object_type=object});}
#else
  #define push(object_type, object) \
    push_object((cognate_object){.type=object_type, .object_type=object})
#endif

#define pop(object_type) \
  check_type(object_type, pop_object()) . object_type

#define peek(object_type) \
  check_type(object_type, peek_object()) . object_type

static void init_stack();
static void push_object(cognate_object);
static cognate_object pop_object();
static cognate_object peek_object();
static void expand_stack();

cognate_list stack;
size_t stack_size;
size_t stack_size_next;

static void init_stack()
{
  //printf("ALLOCATING %i BYTES\n", INITIAL_LIST_SIZE * sizeof(cognate_object));
  // Allocate dynamic stack memory.
  stack.top = stack.start = (cognate_object*) malloc (STACK_SIZE * sizeof(cognate_object));
  stack_size = STACK_SIZE;
  stack_size_next = STACK_SIZE_NEXT;
}

static void push_object(cognate_object object)
{
  if (stack.start + stack_size == stack.top)
      expand_stack();
  *stack.top++ = object;
}

static cognate_object pop_object()
{ 
  if (stack.top == stack.start) throw_error("Stack underflow!");
  return *--stack.top;
}

static cognate_object peek_object()
{
  if (stack.top == stack.start) throw_error("Stack underflow!");
  return *(stack.top - 1);
}

static void expand_stack()
{
  // New stack size = current stack size + previous stack size.

  #ifdef DEBUG
    fprintf(stderr, "[DEBUG]%s:%d -> Expanding list/stack from length %lu to %lu\n", __FILE__, __LINE__, stack_size, stack_size_next); 
  #endif

  stack.start = (cognate_object*) realloc (stack.start, stack_size_next * sizeof(cognate_object));
  stack.top = stack.start + stack_size;

  stack_size      ^= stack_size_next;
  stack_size_next ^= stack_size;
  stack_size      ^= stack_size_next;
  stack_size_next += stack_size;
}

#endif
