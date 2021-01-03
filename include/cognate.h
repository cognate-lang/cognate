#ifndef COGNATE_H
#define COGNATE_H

#define  _GNU_SOURCE

#include <stddef.h>

static const int MAX_TABLE_TRIES = 3;
static const int INITIAL_READ_SIZE = 64;
static const int INITIAL_LIST_SIZE = 16;
static const float LIST_GROWTH_FACTOR = 1.5;

#define program(body) \
  static void run_program() \
    body

#define immutable const
#define mutable __block

// Global-local variable swapping is causing performance losses. :(
#define function(name, flags, docopy, body) \
  flags cognate_block cognate_function_ ## name = make_block(docopy, \
  { \
    const char* const temp_func_name = function_name; \
    function_name = #name; \
    check_call_stack(); \
    body \
    function_name = temp_func_name; \
  });

#define mutate_function(name, docopy, body) \
  cognate_function_ ## name = make_block(docopy, body);

// Internal cognate variable.
#define variable(name, flags) \
  const cognate_object cognate_variable_ ## name = pop_any(); \
  flags cognate_block cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

// Mutate internal variable.
#define mutate_variable(name) \
  __block cognate_object cognate_variable_ ## name = check_block(pop_any()); \
  cognate_function_##name = ^{push_any(cognate_variable_ ## name);}; /* This may break in future and need Block_copy() */
 
#define make_block(docopy, body) \
  ^{ \
    /* Temp variables causes ~10% performance loss :( */ \
    const ptrdiff_t temp_modified = stack.modified; \
    stack.modified = 0; \
    __attribute__((unused)) char if_status = 2; \
    body \
    if (docopy) copy_blocks(); \
    stack.modified = temp_modified; \
  }

#ifndef noGC
  #define cognate_malloc  GC_malloc
  #define cognate_realloc GC_realloc
  #define cognate_malloc_atomic GC_malloc_atomic
#else
  #define cognate_malloc  malloc
  #define cognate_realloc realloc
  #define cognate_malloc_atomic malloc
#endif

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)   (__builtin_expect((_Bool)(expr), 1))

#endif
