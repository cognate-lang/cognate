#ifndef COGNATE_C
#define COGNATE_C

// Macro to define internal cognate function.
// If stack corruption errors occur, try defining functions here as static like with variables.
#define cognate_define(name, body) \
  const __block void(^ cognate_func_ ## name)(void) = ^body

// Macro for defining internal cognate variables.
#define cognate_let(name) \
  cognate_object cognate_variable_ ## name = pop_object(); \
  const void(^ cognate_func_ ## name)(void); cognate_func_ ## name = ^{push_object(cognate_variable_ ## name);};

// This macro attempts to prevent unnecessary use of the return stack. It should only be used at the end of a block.
// Keep for later.
#define attempt_tco(name) \
  if ( &&lbl_call_##name == __builtin_return_address(0) ) \
  { \
    goto lbl_def_##name; \
  } \
  else { \
    cognate_func_##name (); \
    lbl_call_##name:;\
  }

#define tco_call(name) \
  goto lbl_def_##name;

//#define MAX_RECURSION_DEPTH 1048576

#ifdef MAX_RECURSION_DEPTH
static void init_recursion_depth_check();
static void check_recursion_depth();
#endif

#include "debug.c"
#include <time.h>
#include "stack.c"
#include "func.c"
#include "io.c"
#include "error.c"
#include "type.c"

static void init()
{
  #ifdef MAX_RECURSION_DEPTH
  // Mark top of RETURN stack.
  init_recursion_depth_check();
  #endif
  // Seed the random number generator properly.
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  srand(ts.tv_nsec ^ ts.tv_sec);
  // Generate a stack.
  init_stack();
}

#ifdef MAX_RECURSION_DEPTH
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
#endif

#endif
