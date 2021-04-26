#pragma once

#define _GNU_SOURCE

#include <stddef.h>
#include <stdio.h>
#include <Block.h>

#define MAX_TABLE_TRIES    3
#define INITIAL_READ_SIZE  64
#define INITIAL_LIST_SIZE  16
#define INITIAL_TABLE_SIZE 256
#define LIST_GROWTH_FACTOR 1.5
#define STACK_MARGIN_KB    50

typedef void(^BLOCK)();
typedef _Bool BOOLEAN;
typedef double NUMBER;
typedef const char* STRING;
typedef const struct cognate_list* LIST;
typedef _Bool TABLE;
typedef struct cognate_object ANY;

enum cognate_type
{
  // NOTHING is currently only used for unused hashtable buckets.
  NOTHING = 0, // Must be zero because of calloc()
  boolean = (1 << 0),
  string  = (1 << 1),
  number  = (1 << 2),
  list    = (1 << 3),
  table   = (1 << 4),
  block   = (1 << 5),
};

// Enumerates all possible types of a cognate_object
typedef enum cognate_type cognate_type;

typedef struct cognate_object
{
  union
  {
    BOOLEAN boolean;   // 1bit bool
    BLOCK   block;     // 64bit block pointer
    NUMBER  number;    // 64bit float
    STRING  string;    // 64bit string pointer
    LIST    list;      // 64bit list pointer
    TABLE   table;     // TODO
  };
  cognate_type type;
} cognate_object;

typedef struct cognate_list
{
  LIST next;
  cognate_object object;
} cognate_list;

typedef struct cognate_stack
{
  cognate_object* restrict start; // Pointer to start.
  cognate_object* restrict top;   // Pointer to top.
  ptrdiff_t       size;           // Allocated size of the stack.
} cognate_stack;

#define OBJ(objtype, objvalue) ((cognate_object){.type=objtype, .objtype=objvalue})
#define VAR(name) ___##name
#define CHECK_VAR(name) (check_var(#name, ___##name), ___##name)
#define CHECK(typ, obj) (check_type(typ, obj) . typ)
#define CALL(name, args) (set_current_word_name(#name), ___##name args)

#define PREDEF_MUTABLE_DEFINE(name) __block BLOCK ___##name = ^{ throw_error("Function '"#name"' called before definition!'"); };
#define PREDEF_IMMUTABLE_DEFINE(name) PREDEF_MUTABLE_DEFINE(name)

#define PREDEF_MUTABLE_LET(name) __block cognate_object ___##name = (cognate_object){.type=NOTHING};
#define PREDEF_IMMUTABLE_LET(name) cognate_object ___##name = (cognate_object){.type=NOTHING};
#define LET(name, val) ___##name = val;

#define SET(name, val) ___##name = copy_if_block(val);
#define SET_FN(name, val) const ANY _tmp_##name = val; \
                          ___##name = Block_copy(^{push(val);})

#define PROGRAM(body) \
  int main(int argc, char** argv) \
  { \
    init(argc, argv); \
    body;             \
    cleanup(); \
  }

#define MAKE_BLOCK(body) \
  ^{ \
    check_function_stack_size(); \
    body \
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
extern LIST cmdline_parameters;
extern const char *current_function_name;
extern const char *current_word_name;

// Variables and  needed by functions.c defined in runtime.c
void init_stack();
void expand_stack();
void check_var(char*, cognate_object);
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
void set_current_word_name(const char *const);
cognate_object copy_if_block(cognate_object obj);

// Builtin functions needed by compiled source file defined in functions.c
ANY ___if(BLOCK, cognate_object, cognate_object);
void ___while(BLOCK, BLOCK);
void ___do(BLOCK);
void ___put(cognate_object);
void ___print(cognate_object);
NUMBER ___ADD(NUMBER, NUMBER);
NUMBER ___MUL(NUMBER, NUMBER);
NUMBER ___SUB(NUMBER, NUMBER);
NUMBER ___DIV(NUMBER, NUMBER);
NUMBER ___modulo(NUMBER, NUMBER);
NUMBER ___random(NUMBER, NUMBER, NUMBER);
void ___drop(cognate_object);
ANY ___twin(cognate_object);
ANY ___triplet(cognate_object);
ANY ___swap(cognate_object, cognate_object);
void ___clear();
extern ANY ___true;
extern ANY ___false;
BOOLEAN ___either(BOOLEAN, BOOLEAN);
BOOLEAN ___both  (BOOLEAN, BOOLEAN);
BOOLEAN ___one_of(BOOLEAN, BOOLEAN);
BOOLEAN ___not   (BOOLEAN);
BOOLEAN ___EQ(cognate_object, cognate_object);
BOOLEAN ___NEQ(cognate_object, cognate_object);
BOOLEAN ___LT(NUMBER, NUMBER);
BOOLEAN ___GT(NUMBER, NUMBER);
BOOLEAN ___LTE(NUMBER, NUMBER);
BOOLEAN ___GTE(NUMBER, NUMBER);
BOOLEAN ___number_(cognate_object);
BOOLEAN ___list_(cognate_object);
BOOLEAN ___string_(cognate_object);
BOOLEAN ___block_(cognate_object);
BOOLEAN ___boolean_(cognate_object);
ANY ___first(LIST);
LIST ___rest(LIST);
STRING ___head(STRING);
STRING ___tail(STRING);
LIST ___push(cognate_object, LIST);
BOOLEAN ___empty_(LIST);
LIST ___list(BLOCK);
STRING ___join(NUMBER);
NUMBER ___string_length(STRING);
STRING ___substring(NUMBER, NUMBER, STRING);
STRING ___input();
STRING ___read(STRING);
NUMBER ___number(STRING);
STRING ___path();
LIST ___stack();
void ___write(STRING, cognate_object);
LIST ___parameters();
void ___stop();
//TABLE ___table();
//TABLE ___insert(STRING, cognate_object, TABLE);
//ANY ___get(STRING, TABLE);
//LIST ___values(TABLE);
BOOLEAN ___match(STRING, STRING);
NUMBER ___ordinal(STRING);
STRING ___character(NUMBER);
NUMBER ___floor(NUMBER);
NUMBER ___round(NUMBER);
NUMBER ___ceiling(NUMBER);
void ___assert(STRING, BOOLEAN);
void ___error(STRING);
