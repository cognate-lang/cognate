#ifndef COGNATE_C
#define COGNATE_C

#define mutable   __block
#define immutable const

#define copy   1
#define nocopy 0

#define function(name, flags, docopy, body) \
  flags cognate_block cognate_function_ ## name = make_block(docopy, body);

#define malloc  GC_MALLOC
#define realloc GC_REALLOC

#define mutate_function(name, docopy, body) \
  cognate_function_ ## name = make_block(docopy, body);

#define variable(name, flags) \
  immutable cognate_object cognate_variable_ ## name = pop_any(); \
  flags cognate_block cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

#define mutate_variable(name) \
  immutable cognate_object cognate_variable_ ## name = check_block(pop_any()); \
  cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

  
#define make_block(docopy, body) \
  ^{ \
    /* Temp variable causes ~10% performance loss :( */\
    const ptrdiff_t temp_modified = stack.modified - stack.items.start; \
    stack.modified = stack.items.top; \
    body \
    if (docopy) copy_blocks(); \
    stack.modified = temp_modified + stack.items.start; \
  }

/*
#define MAX_RECURSION_DEPTH 1048576

static void init_recursion_depth_check();
static void check_recursion_depth();
*/

#include <time.h>
#include "stack.c"
#include "func.c"
#include "io.c"
#include "error.c"
#include "type.c"
#include "record.c"
#include <limits.h>

static void init()
{
  // Mark top of RETURN stack (for recursion depth checking).
  //init_recursion_depth_check();
  // Seed the random number generator properly.
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  srand(ts.tv_nsec ^ ts.tv_sec);
  // Generate a stack.
  init_stack();
}

/*
static char* return_stack_start;
static void check_recursion_depth()
{
  // Gets the size of the return stack (not cognate's stack!).
  // Can be used to 'catch' stack overflow errors.
  char var; // Address of local var is top of return stack!!!
  int return_stack_size = (&var) - return_stack_start;
  if (return_stack_size > MAX_RECURSION_DEPTH || return_stack_size < -MAX_RECURSION_DEPTH)
  {
    throw_error("Maximum recursion depth exceeded!");
  }
}
static void init_recursion_depth_check()
{
  char var;
  return_stack_start = &var;
}
*/

void copy_blocks()
{
  //printf("Scanning %lu items\n", stack.top-stack_modified);
  for (;stack.modified < stack.items.top; ++stack.modified)
  {
    if (stack.modified->type==block)
    {
      stack.modified->block = Block_copy(stack.modified->block); // Copy block to heap.
    }
  }
}

cognate_object check_block(cognate_object obj)
{
  if (obj.type==block)
    obj.block=Block_copy(obj.block);
  return obj;
}

#endif
