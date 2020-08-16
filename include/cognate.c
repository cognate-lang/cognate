#ifndef COGNATE_C
#define COGNATE_C

#define mutable   __block
#define immutable const

#define function(name, flags, body) \
  flags cognate_block cognate_function_ ## name = safe_block(body);

#define unsafe_function(name, flags, body) \
  flags cognate_block cognate_function_ ## name = unsafe_block(body);

#define malloc  GC_MALLOC
#define realloc GC_REALLOC

#define mutate_function(name, body) \
  cognate_function_ ## name = safe_block(body);

#define variable(name, flags) \
  immutable cognate_object cognate_variable_ ## name = pop_any(); \
  flags cognate_block cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

#define mutate_variable(name) \
  immutable cognate_object cognate_variable_ ## name = check_block(pop_any()); \
  cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};
  
#define unsafe_block(body) \
({ \
  cognate_block blk = \
  ^{ \
    body \
    cognate_object *temp_thing = stack_uncopied; \
    stack_uncopied = stack.top; \
    copy_blocks(); \
    stack_uncopied = temp_thing; \
  }; \
  blk; \
})

#define safe_block(body) \
({ \
  cognate_block blk = \
  ^{ \
    cognate_object *temp_thing = stack_uncopied; \
    stack_uncopied = stack.top; \
    body \
    copy_blocks(); \
    stack_uncopied = temp_thing; \
  }; \
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
/*
int *block_depth(cognate_block blk)
{
  return (int*)blk+3; 
}
*/
cognate_block copy_block(cognate_block blk)
{
  blk = Block_copy(blk);
  return blk;
}

void copy_blocks()
{
  // When this prints a huge negative number, the stack has probably overflown and corrupted stack_uncopied.
  printf("Scanning %li items\n", stack.top - stack_uncopied);
  for (;stack_uncopied < stack.top; ++stack_uncopied)
  {
    if (stack_uncopied->type==block)
    {
      puts("Copying block");
      stack_uncopied->block = copy_block(stack_uncopied->block); // Copy block to heap.
      puts("Copied block");
    }
  }
  stack_uncopied = stack.top;
}

cognate_object check_block(cognate_object obj)
{
  if (obj.type==block)
    obj.block=copy_block(obj.block);
  return obj;
}

#endif
