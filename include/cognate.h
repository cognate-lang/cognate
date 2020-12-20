#ifndef COGNATE_H
#define COGNATE_H

#define  _GNU_SOURCE

#include <limits.h>

#define MAX_TABLE_TRIES 3
#define INITIAL_READ_SIZE 64
#define MIN_TABLE_SIZE 2
#define INITIAL_LIST_SIZE 16
#define LIST_GROWTH_FACTOR 1.5

#define program(body) \
  int main(int argc, char **argv) \
  { \
    init(argc, argv); \
    __attribute__((unused)) char if_status = 2; \
    body \
    cleanup(); \
    return 0; \
  }


#define immutable const
#define mutable __block

#define copy   1
#define nocopy 0

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
  immutable cognate_object cognate_variable_ ## name = pop_any(); \
  flags cognate_block cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

// Mutate internal variable.
#define mutate_variable(name) \
  immutable cognate_object cognate_variable_ ## name = check_block(pop_any()); /* Can't remember what check_block does here */\
  cognate_function_##name = ^{push_any(cognate_variable_ ## name);};
 
#define make_block(docopy, body) \
  ^{ \
    /* Temp variables causes ~10% performance loss :( */ \
    const ptrdiff_t temp_modified = stack.modified - stack.items.start; \
    stack.modified = stack.items.top; \
    __attribute__((unused)) char if_status = 2; \
    body \
    if (docopy) copy_blocks(); \
    stack.modified = temp_modified + stack.items.start; \
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
