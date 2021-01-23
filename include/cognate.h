#ifndef COGNATE_H
#define COGNATE_H

#define  _GNU_SOURCE

#include <stddef.h>

#define MAX_TABLE_TRIES 3
#define INITIAL_READ_SIZE 64
#define INITIAL_LIST_SIZE 16
#define LIST_GROWTH_FACTOR 1.5

#define DIGITS 14

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
  const cognate_object cognate_variable_ ## name = check_block(pop_any()); \
  flags cognate_block cognate_function_ ## name = ^{ push_any(cognate_variable_ ## name); };

// Mutate internal variable.
#define mutate_variable(name) \
  const cognate_object cognate_variable_ ## name = check_block(pop_any()); \
  cognate_function_##name = Block_copy(^{ push_any(cognate_variable_ ## name); }); // fsanitize=address goes crazy here.

#define make_block(docopy, body) \
  ^{ \
    __attribute__((unused)) char if_status = 2; \
    body \
    copy_blocks(); \
  }

#ifdef noGC
  #define GC_MALLOC_ATOMIC malloc
  #define GC_MALLOC  malloc
  #define GC_REALLOC realloc
  #define GC_STRNDUP strndup
  #define GC_STRDUP  strdup
  #define GC_NEW(t)  ((t*) malloc (sizeof(t)))
#endif

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)   (__builtin_expect((_Bool)(expr), 1))

#define FOR_LIST(i, name) \
  for (const cognate_list* i = (name) ; i ; i = i->next)


#endif
