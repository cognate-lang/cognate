#ifndef STACK_C
#define STACK_C

#include "cognate.h"
#include "types.h"

static void init_stack();
static void push_any(const cognate_object);
static cognate_object pop_any();
static cognate_object peek_any();
static void expand_stack();

#ifdef debug // Push an object to the stack. Print if debugging.
  #define push(object_type, object) \
    debug_printf("Pushing %s", #object); \
    push_any((cognate_object){.object_type=object, .type=object_type});
#else
  #define push(object_type, object) \
    push_any((cognate_object){.object_type=object, .type=object_type})
#endif

#define pop(object_type) \
  (check_type(object_type, pop_any()) . object_type)

#define peek(object_type) \
  (check_type(object_type, peek_any()) . object_type)

#include "error.c"

#include <gc/gc.h>
#include <stdio.h>

struct cognate_stack
{
  cognate_list items;    // The list holding the stack itself.
  ptrdiff_t    size;     // Allocated size of the stack.
  ptrdiff_t    modified; // Offset of last modified element from top.
};

typedef struct cognate_stack cognate_stack;

static cognate_stack stack;

static void init_stack()
{
  // Allocate dynamic stack memory.
  stack.modified = 0;
  stack.items.top = stack.items.start =
    (cognate_object*) cognate_malloc ((stack.size = INITIAL_LIST_SIZE) * sizeof(cognate_object));
}

static void push_any(const cognate_object object)
{
  // Profiles says that this function is The Problem.
  // builtin_expect optimises because the stack hardly ever needs to expand.
  if unlikely(stack.items.start + stack.size == stack.items.top)
    expand_stack();
  *stack.items.top++ = object;
}

static cognate_object pop_any()
{ 
  if unlikely(stack.items.top == stack.items.start)
    throw_error("Stack underflow!");
  stack.modified += (stack.modified == 0);
  return *--stack.items.top;
}

static cognate_object peek_any()
{
  if unlikely(stack.items.top == stack.items.start)
    throw_error("Stack underflow!");
  return *(stack.items.top - 1);
}

static void expand_stack()
{
  // New stack size = current stack size * growth factor.
  // Assumes that stack is currently of length stack.size.
  debug_printf("Expanding stack from length %ti to %ti\n", stack.size, (ptrdiff_t)(stack.size * LIST_GROWTH_FACTOR)); 
  stack.items.start = (cognate_object*) cognate_realloc (stack.items.start, (ptrdiff_t)(stack.size * LIST_GROWTH_FACTOR * sizeof(cognate_object)));
  stack.items.top = stack.items.start + stack.size;
  stack.size *= LIST_GROWTH_FACTOR;
}

#endif
