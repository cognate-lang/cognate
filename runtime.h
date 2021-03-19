#define _GNU_SOURCE

#ifndef COGNATE_RUNTIME_H
#define COGNATE_RUNTIME_H

#include <stddef.h>

#define MAX_TABLE_TRIES    3
#define INITIAL_READ_SIZE  64
#define INITIAL_LIST_SIZE  16
#define INITIAL_TABLE_SIZE 256
#define LIST_GROWTH_FACTOR 1.5
#define STACK_MARGIN_KB    50

// A pointer to a lexical closure
typedef void(^cognate_block)();

// A boolean value
typedef _Bool cognate_boolean;

// A double-precision floating point number
typedef double cognate_number;

// A multibyte null-terminated string
typedef const char* cognate_string;

// A linked list of cognate_objects
typedef const struct cognate_list_node* cognate_list;

// A map of string keys to cognate_object values TODO
typedef _Bool cognate_table;

// Stores values for variables and stack items
typedef struct cognate_object cognate_object;

// A stack which can be pushed to and popped from
typedef struct cognate_stack  cognate_stack;

// A single node in a cognate_list
typedef struct cognate_list_node cognate_list_node;

enum cognate_type
{
  // NOTHING is currently only used for unused hashtable buckets.
  NOTHING = 0, // Must be zero because of calloc()
  boolean = (1 << 0),
  string = (1 << 1),
  number = (1 << 2),
  list = (1 << 3),
  table = (1 << 4),
  block = (1 << 5),
  heap_block = (1 << 5) | (1 << 6)
};

// Enumerates all possible types of a cognate_object
typedef enum cognate_type cognate_type;

struct cognate_object
{
  union
  {
    cognate_boolean boolean;   // 1bit bool
    cognate_block   block;     // 64bit block pointer
    cognate_block   heap_block;// 64bit block pointer
    cognate_number  number;    // 64bit float
    cognate_string  string;    // 64bit string pointer
    cognate_list    list;      // 64bit list pointer
    cognate_table   table;     // TODO
  };
  cognate_type type;
};

struct cognate_list_node
{
  cognate_list next;
  cognate_object object;
};

struct cognate_stack
{
  cognate_object* restrict start; // Pointer to start.
  cognate_object* restrict top; // Pointer to top.
  ptrdiff_t       size; // Allocated size of the stack.
  size_t          uncopied_blocks; // Number of uncopied cognate_blocks on the stack.
};

#define immutable const
#define mutable __block

#define OBJ(objtype, objvalue) ((cognate_object){.type=objtype, .objtype=objvalue})
#define VAR(name) ___##name
#define CHECK(typ, obj) (check_type(typ, obj) . typ)
#define CALL(name, args) (set_current_word_name(#name), ___##name args)

#define DEFINE(flags, name, body) \
  flags cognate_block ___##name = \
  BLOCK( \
    const char* const temp_function_name = current_function_name; \
    current_function_name = #name; \
    body;                                   \
    current_function_name = temp_function_name; \
  );

#define REDEFINE(name, body) ___##name = Block_copy(BLOCK(docopy, body));

#define LET(flags, name, val) flags cognate_object ___##name = copy_if_block(val);

#define SET(name, val) ___##name = copy_if_block(val);

#define PROGRAM(body) \
  int main(int argc, char** argv) \
  { \
    init(argc, argv); \
    body;             \
    cleanup(); \
  }

#define BLOCK(body) \
  ^{ \
    check_function_stack(); \
    body \
    copy_stack_blocks(); \
  }

#ifdef NO_GC
  #define GC_MALLOC  malloc
  #define GC_REALLOC realloc
  #define GC_STRNDUP strndup
  #define GC_STRDUP  strdup
  #define GC_NEW(t)  ((t*) malloc (sizeof(t)))
#endif

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)   (__builtin_expect((_Bool)(expr), 1))

#endif
