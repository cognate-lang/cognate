#ifndef COGNATE_C
#define COGNATE_C

// Macro to define internal cognate function.
// __block attribute allows recursion and mutation at performance cost.

int scope_depth = 0;

#define mutable  __block
#define immutable const

#define function(name, flags, body) \
  flags cognate_block cognate_function_ ## name = safe_block(body);

#define unsafe_function(name, flags, body) \
  flags cognate_block cognate_function_ ## name = unsafe_block(body);

#define malloc GC_MALLOC
#define realloc GC_REALLOC

#define mutate_function(name, body) \
  cognate_function_ ## name = safe_block(body);

// Macro for defining internal cognate variables.
// __block attribute allows mutation at performance cost.
#define variable(name, flags) \
  immutable cognate_object cognate_variable_ ## name = pop_any(); \
  flags cognate_block cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

#define mutate_variable(name) \
  immutable cognate_object cognate_variable_ ## name = check_block(pop_any()); \
  cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};
  
#define safe_block(body) \
({ \
  cognate_block blk = ^{ \
    scope_depth++; \
    body \
    scope_depth--; \
    copy_blocks(); \
  }; \
  int* x = (int*)blk + 3; \
  *x = scope_depth; \
  blk; \
})

#define unsafe_block(body) \
({ \
  cognate_block blk = ^{ \
    scope_depth++; \
    body \
    scope_depth--; \
  }; \
  int* x = (int*)blk + 3; \
  *x = scope_depth; \
  blk; \
})


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
  for (cognate_object *i = stack_uncopied; i < stack.top; ++i)
  {
    if (i->type==block)
    {
      int* depth = (int*)i->block + 3; // Scope depth when block declared.
      if (*depth > scope_depth)
      {
        i->block = Block_copy(i->block); // Copy block to heap.
        *depth = 0; // If depth is zero, block will never be copied.
        // Setting depth to zero after copying prevents copying again. 
      } //else break; // Remove the else break.
    }
  }
  stack_uncopied = stack.top;
}

cognate_object check_block(cognate_object obj)
{
  if (obj.type==block)
    obj.block=Block_copy(obj.block);
  return obj;
}

#endif
