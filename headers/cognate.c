#ifndef COGNATE_C
#define COGNATE_C

#include <time.h>
#include "stack.c"
#include "func.c"
#include "io.c"
#include "error.c"
#include "type.c"

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


static void init()
{
  // Seed the random number generator properly.
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  srand(ts.tv_nsec ^ ts.tv_sec);
  // Generate a stack.
  init_stack();
}

static void cleanup()
{
  // Do stuff.
}
#endif
