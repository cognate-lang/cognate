#ifndef COGNATE_C
#define COGNATE_C

// Macro to define internal cognate function.
// __block attribute allows recursion and mutation at performance cost.

#define mutable const __block
#define immutable const

#define function(name, flags, body) \
  flags void(^ cognate_function_ ## name)(void) = ^body

#define malloc GC_MALLOC
#define realloc GC_REALLOC

#define mutate_function(name, body) \
  cognate_function_ ## name = ^body

// Macro for defining internal cognate variables.
// __block attribute allows mutation at performance cost.
#define variable(name, flags) \
  immutable cognate_object cognate_variable_ ## name = pop_any(); \
  flags void(^ cognate_function_ ## name)(void) = ^{push_any(cognate_variable_ ## name);};

#define mutate_variable(name) \
  const cognate_object cognate_variable_ ## name = pop_any(); \
  cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

#define record(name, size) \
  int this_type = next_type++; \
  immutable void(^ cognate_function_ ## name)(void) = \
  ^{ \
    cognate_object *rec = (cognate_object*) malloc (sizeof(cognate_object) * size); \
    for (int i = 0; i < size; ++i) \
    { \
      rec[i] = pop_any(); \
    } \
    push_any((cognate_object){.type=this_type, .record=rec}); \
  }; \
  immutable void(^ cognate_function_is_ ## name)(void) = \
  ^{ \
    push(boolean, pop_any().type == this_type); \
  }

// Field definitions MUST come directly after record definitions
// because each record definition increments the type counter.

#define field(name, pos) \
  immutable void(^ cognate_function_ ## name)(void) = \
  ^{ \
    cognate_object rec = pop_any(); \
    if (rec.type == this_type) { push_any(rec.record[pos]); } \
    else { throw_error("No field '"#name"' in record!"); } \
  };


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
//#include <setjmp.h>

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

#endif
