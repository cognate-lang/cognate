#ifndef COGNATE_H
#define COGNATE_H

#define  _GNU_SOURCE

#include <stddef.h>

#define MAX_TABLE_TRIES 3
#define INITIAL_READ_SIZE 64
#define INITIAL_LIST_SIZE 16
#define INITIAL_TABLE_SIZE 256
#define LIST_GROWTH_FACTOR 1.5

#define OBJ(objtype, objvalue) ((cognate_object){.type=objtype, .objtype=objvalue})
#define VAR(name) ___##name
#define CHECK(typ, obj) (check_type(typ, obj) . typ)

#define PROGRAM(body) \
  int main(int argc, char** argv) \
  { \
    init(argc, argv); \
    body \
    cleanup(); \
  }

#define immutable const
#define mutable __block

// Global-local variable swapping is causing performance losses. :(
#define DEFINE(flags, name, body) \
  flags cognate_block ___##name = \
  BLOCK( \
    const char* const temp_func_name = function_name; \
    function_name = #name; \
    check_call_stack(); \
    body \
    function_name = temp_func_name; \
  );

#define mutate_function(name, body) \
  ___##name = Block_copy(BLOCK(docopy, body));

// Internal cognate variable.
#define LET(flags, name, val) \
  flags cognate_object ___ ## name = check_block(val); \

// Mutate internal variable.
#define SET(name, val) \
  ___##name = check_block(val); // fsanitize=address goes crazy here.

#define BLOCK(body) \
  ^{ \
    body \
    copy_blocks(); \
  }

// May need to use calloc() in future, if i start relying on GC_MALLOC zeroing memory.
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


#endif
