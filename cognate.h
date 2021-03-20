#pragma once

#define _GNU_SOURCE

#include <stddef.h>
#include <stdio.h>

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

// Macros
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
    check_function_stack_size(); \
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

#define DOIF(cond, a, b) \
  cond; \
  if (CHECK(boolean, pop())) \
  a else b

// Global variables
extern cognate_stack stack;
extern cognate_list cmdline_parameters;
extern const char *current_function_name;
extern const char *current_word_name;

// Variables and  needed by functions.c defined in runtime.c
void init_stack();
void expand_stack();
void print_object(const cognate_object object, FILE *, const _Bool);
void _Noreturn __attribute__((format(printf, 1, 2))) throw_error(const char *const, ...);
_Bool compare_objects(cognate_object, cognate_object);

// Variables and functions needed by compiled source file defined in runtime.c
void init(int, char **);
void cleanup();
cognate_object check_type(cognate_type, cognate_object);
void push(cognate_object);
cognate_object pop();
cognate_object peek();
void check_function_stack_size();
void copy_stack_blocks();
void set_current_word_name(const char *const);
cognate_object copy_if_block(cognate_object obj);

// Builtin functions needed by compiled source file defined in functions.c
void ___if(cognate_block, cognate_object, cognate_object);
void ___while(cognate_block, cognate_block);
void ___do(cognate_block);
void ___put(cognate_object);
void ___print(cognate_object);
cognate_number ___sum(cognate_number, cognate_number);
cognate_number ___multiply(cognate_number, cognate_number);
cognate_number ___subtract(cognate_number, cognate_number);
cognate_number ___divide(cognate_number, cognate_number);
cognate_number ___modulo(cognate_number, cognate_number);
cognate_number ___random(cognate_number, cognate_number, cognate_number);
void ___drop(cognate_object);
void ___twin(cognate_object);
void ___triplet(cognate_object);
void ___swap(cognate_object, cognate_object);
void ___clear();
cognate_boolean ___true();
cognate_boolean ___false();
cognate_boolean ___either(cognate_boolean, cognate_boolean);
cognate_boolean ___both  (cognate_boolean, cognate_boolean);
cognate_boolean ___one_of(cognate_boolean, cognate_boolean);
cognate_boolean ___not   (cognate_boolean);
cognate_boolean ___equal(cognate_object, cognate_object);
cognate_boolean ___unequal(cognate_object, cognate_object);
cognate_boolean ___exceed(cognate_number, cognate_number);
cognate_boolean ___preceed(cognate_number, cognate_number);
cognate_boolean ___equalorpreceed(cognate_number, cognate_number);
cognate_boolean ___equalorexceed(cognate_number, cognate_number);
cognate_boolean ___number_(cognate_object);
cognate_boolean ___list_(cognate_object);
cognate_boolean ___string_(cognate_object);
cognate_boolean ___block_(cognate_object);
cognate_boolean ___boolean_(cognate_object);
void ___first(cognate_list);
cognate_list ___rest(cognate_list);
cognate_string ___head(cognate_string);
cognate_string ___tail(cognate_string);
cognate_list ___push(cognate_object, cognate_list);
cognate_boolean ___empty_(cognate_list);
cognate_list ___list(cognate_block);
void ___characters();
void ___split();
void ___join(cognate_number);
cognate_number ___string_length(cognate_string);
cognate_string ___substring(cognate_number, cognate_number, cognate_string);
cognate_string ___input();
cognate_string ___read(cognate_string);
cognate_number ___number(cognate_string);
cognate_string ___path();
void ___stack();
void ___write(cognate_string, cognate_object);
cognate_list ___parameters();
void ___stop();
cognate_table ___table();
cognate_table ___insert(cognate_string, cognate_object, cognate_table);
void ___get(cognate_string, cognate_table);
cognate_list ___values(cognate_table);
cognate_boolean ___match(cognate_string, cognate_string);
cognate_number ___ordinal(cognate_string);
cognate_string ___character(cognate_number);
cognate_number ___floor(cognate_number);
cognate_number ___round(cognate_number);
cognate_number ___ceiling(cognate_number);
void ___assert(cognate_string, cognate_boolean);
void ___error(cognate_string);
