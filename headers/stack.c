#ifndef STACK_C
#define STACK_C

#include <stdio.h>
#include "type.c"
#include <stdlib.h>
#include "gc.h"

#define INITIAL_LIST_SIZE 8
// Make 8 when I have fixed stack expansion

#define push(object_type, object) \
  push_object((cognate_object){.type=object_type, .object_type=object})

#define pop(object_type) \
  check_type(object_type, pop_object()) . object_type

#define peek(object_type) \
  check_type(object_type, peek_object()) . object_type

static void init_stack(void);
static void push_object(cognate_object);
static cognate_object pop_object(void);
static cognate_object peek_object(void);
static void expand_stack(size_t);

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
  // This was expanding the stack when it wasn't needed :(
 
  if (stack.top == stack_end)
    expand_stack((stack.top - stack.start) << 1);

  *stack.top++ = object;
}

static cognate_object pop_object(void)
{ 

  //printf("POPPING");
  if (stack.top == stack.start) throw_error("Stack underflow!");
  //printf("DONE POPPING");
  return *--stack.top;
}

static cognate_object peek_object(void)
{
  if (stack.top == stack.start) throw_error("Stack underflow!");

  return *(stack.top - 1);
}

static void expand_stack(size_t new_size)
{
  // ASSUMES THAT THE STACK IS FULL!!!
  // Valgrind throws invalid write errors here.
  size_t old_size = stack.top - stack.start;
  stack.start = (cognate_object*) realloc (stack.start, new_size * sizeof(cognate_object));
  stack.top = stack.start + old_size;
  stack_end = stack.start + new_size;
}

#endif
