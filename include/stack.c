#ifndef STACK_C
#define STACK_C

#include "cognate.h"
#include "type.c"
#include <stdio.h>
#include <stdlib.h>

#define INITIAL_LIST_SIZE 16 // Constant values for initialising stack sizes.
#define LIST_GROWTH_FACTOR 1.5

#ifdef debug // Push an object to the stack. Print if debugging.
  #define push(object_type, object) \
    debug_printf("Pushing %s", #object); \
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
  // Allocate dynamic stack memory.
  stack.modified = stack.items.top = stack.items.start = (cognate_object*) cognate_malloc ((stack.size = INITIAL_LIST_SIZE) * sizeof(cognate_object));
}

static void push_any(cognate_object object)
{
  // Profiles says that this function is The Problem.
  // builtin_expect optimises because the stack hardly ever needs to expand.
  if (unlikely(stack.items.start + stack.size == stack.items.top))
    expand_stack();
  *stack.items.top++ = object;
}

static cognate_object pop_any()
{ 
  if (unlikely(stack.items.top == stack.items.start))
    throw_error("Stack underflow!");
  stack.modified -= (stack.modified == stack.items.top);
  return *--stack.items.top;
}

static cognate_object peek_any()
{
  if (unlikely(stack.items.top == stack.items.start))
    throw_error("Stack underflow!");
  return *(stack.items.top - 1);
}

static void expand_stack()
{
  // New stack size = current stack size * growth factor.

  debug_printf("Expanding stack from length %lu to %lu\n", stack.size, (size_t)(stack.size * LIST_GROWTH_FACTOR)); 
 
  ptrdiff_t temp = stack.modified - stack.items.start;
  stack.items.start = (cognate_object*) cognate_realloc (stack.items.start, (size_t)(stack.size * LIST_GROWTH_FACTOR * sizeof(cognate_object)));
  stack.modified = stack.items.start + temp;
  stack.items.top = stack.items.start + stack.size;
  stack.size *= LIST_GROWTH_FACTOR;
}

#endif
