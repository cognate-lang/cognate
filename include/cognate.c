#ifndef COGNATE_C
#define COGNATE_C

#define mutable   __block
#define immutable const

#define copy   1
#define nocopy 0

// Global-local variable swapping is causing performance losses. :(
#ifdef unsafe
#define function(name, flags, docopy, body) \
  flags cognate_block cognate_function_ ## name = make_block(docopy, body);
#else
#define function(name, flags, docopy, body) \
  flags cognate_block cognate_function_ ## name = make_block(docopy, {char* temp_func_name = function_name; \
                                                                      function_name = #name; \
                                                                      body \
                                                                      function_name = temp_func_name;});
#endif

/*
#define mutate_function(name, docopy, body) \
  cognate_function_ ## name = make_block(docopy, body);
*/

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

#include <time.h>
#include "stack.c"
#include "func.c"
#include "io.c"
#include "error.c"
#include "type.c"
#include "record.c"


static void init(int argc, char** argv)
{
  // Seed the random number generator properly.
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  srand(ts.tv_nsec ^ ts.tv_sec);
  // Generate a stack.
  init_stack();
  params.start = (cognate_object*) GC_MALLOC (sizeof(cognate_object) * (argc-1));
  params.top = params.start + argc - 1;
  for (; argc >= 1; --argc)
  {
    char* str = argv[argc];
    params.start[argc-1] = (cognate_object){.type=string, .string=str};
  }
}

cognate_object check_block(cognate_object obj)
{
  (obj.type==block) && (obj.block = Block_copy(obj.block));
  return obj;
}


void copy_blocks()
{
  while (stack.modified != stack.items.top)
  {
    (stack.modified->type==block) && (stack.modified->block = Block_copy(stack.modified->block)); // Copy block to heap.
    stack.modified++;
  }
}

#endif
