#pragma once

#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2

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
typedef size_t SYMBOL;
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
  symbol =  (1 << 6),
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
    SYMBOL  symbol;
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

extern const char* symtable[];

#define OBJ(objtype, objvalue) ((cognate_object){.type=objtype, .objtype=objvalue})
#define VAR(name) ___##name
#define SYM(name) ____##name
#define CHECK(typ, obj) (check_type(typ, obj) . typ)
#define CALL(name, args) (set_current_word_name(#name), VAR(name) args)

#define PREDEF(name) __block BLOCK VAR(name) = ^{ throw_error("Function '"#name"' called before definition!'"); };

#define SET(name, val) VAR(name) = copy_if_block(val);
#define SET_FN(name, val) const ANY _tmp_##name = val; \
                          VAR(name) = Block_copy(^{ push(val); })

#ifdef NO_GC
  #define GC_MALLOC  malloc
  #define GC_REALLOC realloc
  #define GC_STRNDUP strndup
  #define GC_STRDUP  strdup
  #define GC_NEW(t)  ((t*) malloc (sizeof(t)))
#endif

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)   (__builtin_expect((_Bool)(expr), 1))

// Global variables
extern cognate_stack stack;
extern LIST cmdline_parameters;
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
void set_current_word_name(const char *const);
cognate_object copy_if_block(cognate_object obj);

// Builtin functions needed by compiled source file defined in functions.c
ANY VAR(if)(BLOCK, cognate_object, cognate_object);
void VAR(while)(BLOCK, BLOCK);
void VAR(do)(BLOCK);
void VAR(put)(cognate_object);
void VAR(print)(cognate_object);
NUMBER VAR(ADD)(NUMBER, NUMBER);
NUMBER VAR(MUL)(NUMBER, NUMBER);
NUMBER VAR(SUB)(NUMBER, NUMBER);
NUMBER VAR(DIV)(NUMBER, NUMBER);
NUMBER VAR(modulo)(NUMBER, NUMBER);
NUMBER VAR(random)(NUMBER, NUMBER, NUMBER);
void VAR(clear)();
extern ANY VAR(true);
extern ANY VAR(false);
BOOLEAN VAR(either)(BOOLEAN, BOOLEAN);
BOOLEAN VAR(both)(BOOLEAN, BOOLEAN);
BOOLEAN VAR(one_of)(BOOLEAN, BOOLEAN);
BOOLEAN VAR(not)(BOOLEAN);
BOOLEAN VAR(EQ)(cognate_object, cognate_object);
BOOLEAN VAR(NEQ)(cognate_object, cognate_object);
BOOLEAN VAR(LT)(NUMBER, NUMBER);
BOOLEAN VAR(GT)(NUMBER, NUMBER);
BOOLEAN VAR(LTE)(NUMBER, NUMBER);
BOOLEAN VAR(GTE)(NUMBER, NUMBER);
BOOLEAN VAR(number_)(cognate_object);
BOOLEAN VAR(list_)(cognate_object);
BOOLEAN VAR(string_)(cognate_object);
BOOLEAN VAR(block_)(cognate_object);
BOOLEAN VAR(boolean_)(cognate_object);
ANY VAR(first)(LIST);
LIST VAR(rest)(LIST);
STRING VAR(head)(STRING);
STRING VAR(tail)(STRING);
LIST VAR(push)(cognate_object, LIST);
BOOLEAN VAR(empty_)(LIST);
LIST VAR(list)(BLOCK);
STRING VAR(join)(NUMBER);
NUMBER VAR(string_length)(STRING);
STRING VAR(substring)(NUMBER, NUMBER, STRING);
STRING VAR(input)();
STRING VAR(read)(STRING);
NUMBER VAR(number)(STRING);
STRING VAR(path)();
LIST VAR(stack)();
void VAR(write)(STRING, cognate_object);
LIST VAR(parameters)();
void VAR(stop)();
//TABLE VAR(table)();
//TABLE VAR(insert)(STRING, cognate_object, TABLE);
//ANY VAR(get)(STRING, TABLE);
//LIST VAR(values)(TABLE);
BOOLEAN VAR(match)(STRING, STRING);
NUMBER VAR(ordinal)(STRING);
STRING VAR(character)(NUMBER);
NUMBER VAR(floor)(NUMBER);
NUMBER VAR(round)(NUMBER);
NUMBER VAR(ceiling)(NUMBER);
void VAR(assert)(STRING, BOOLEAN);
void VAR(error)(STRING);
LIST VAR(map)(BLOCK, LIST);
LIST VAR(filter)(BLOCK, LIST);
void VAR(for)(LIST, BLOCK);
LIST VAR(range)(NUMBER, NUMBER, NUMBER);
