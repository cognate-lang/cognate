#pragma once

#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2

#include <stddef.h>
#include <stdio.h>

#define INITIAL_READ_SIZE  64
#define INITIAL_LIST_SIZE  16
#define LIST_GROWTH_FACTOR 1.5
#define STACK_MARGIN_KB    50

// Put the restrict qualifiers back when llvn can stop their compiler segfaulting
typedef void(^BLOCK)();
typedef _Bool BOOLEAN;
typedef double NUMBER;
typedef const char* STRING;
typedef const struct cognate_list*  LIST;
typedef struct cognate_record* RECORD;
typedef const char* SYMBOL;
typedef struct cognate_object ANY;

enum cognate_type
{
  NOTHING = 0, // Must be zero because of calloc()
  boolean = (1 << 0),
  string  = (1 << 1),
  number  = (1 << 2),
  list    = (1 << 3),
  record  = (1 << 4),
  block   = (1 << 5),
  symbol  = (1 << 6),
};

// Enumerates all possible types of a cognate_object
typedef enum cognate_type cognate_type;

typedef struct cognate_object
{
  union
  {
    BOOLEAN boolean; // 1bit bool
    BLOCK   block;   // 64bit block pointer
    NUMBER  number;  // 64bit float
    STRING  string;  // 64bit string pointer
    LIST    list;    // 64bit list pointer
    RECORD  record;
    SYMBOL  symbol;
    long binary_representation;
  };
  cognate_type type;
} cognate_object;

typedef struct cognate_list
{
  LIST restrict next;
  ANY object;
} cognate_list;

typedef struct cognate_record
{
  size_t len;
  struct
  {
    SYMBOL name;
    ANY object;
  } items [1];
} cognate_record;

typedef struct cognate_stack
{
  ANY* restrict start; // Pointer to start.
  ANY* restrict top;   // Pointer to top.
  ptrdiff_t     size;  // Allocated size of the stack.
  ANY cache;
} cognate_stack;

struct Block_descriptor {
    unsigned long int reserved;
    unsigned long int size;
};

struct Block_layout {
    void *isa;
    int reserved;
    long also_reserved;
    struct Block_descriptor *descriptor;
};

#define OBJ(objtype, objvalue) ((ANY){.type=objtype, .objtype=objvalue})
#define VAR(name) ___##name
#define SYM(name) ____##name
#define CHECK(typ, obj) (check_type(typ, obj) . typ)
#define CALL(name, args) VAR(name) args
#define CALLDEBUG(name, args) (set_word_name(#name), set_line_num(__LINE__), VAR(name) args)

#define PREDEF(name) __block BLOCK VAR(name) = ^{ throw_error("Function '"#name"' called before definition!'"); };

#define SET(name, val) VAR(name) = copy_if_block(val);
#define SET_FN(name, val) const ANY _tmp_##name = val; \
                          VAR(name) = Block_copy(^{ push(val); })

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)   (__builtin_expect((_Bool)(expr), 1))

// Global variables
extern cognate_stack stack;
extern LIST cmdline_parameters;
extern const char* restrict word_name;
extern int line_num;

// Variables and  needed by functions.c defined in runtime.c
void* Block_copy(const void*);
void init_stack();
void expand_stack();
void print_object(const ANY object, FILE *, const _Bool);
void _Noreturn __attribute__((format(printf, 1, 2))) throw_error_fmt(const char* restrict const, ...);
void _Noreturn throw_error(const char* restrict const);
_Bool compare_objects(ANY, ANY);

// Variables and functions needed by compiled source file defined in runtime.c
void init(int, char **);
void cleanup();
ANY check_type(cognate_type, ANY);
void push(ANY);
ANY pop();
ANY peek();
int stack_length();
void check_function_stack_size();
void set_word_name(const char* restrict const);
void set_line_num(int);
ANY copy_if_block(ANY obj);

// Builtin functions needed by compiled source file defined in functions.c
ANY VAR(if)(BLOCK, ANY, ANY);
void VAR(while)(BLOCK, BLOCK);
void VAR(do)(BLOCK);
void VAR(put)(ANY);
void VAR(print)(ANY);
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
BOOLEAN VAR(oneDASHof)(BOOLEAN, BOOLEAN);
BOOLEAN VAR(not)(BOOLEAN);
BOOLEAN VAR(EQ)(ANY, ANY);
BOOLEAN VAR(NEQ)(ANY, ANY);
BOOLEAN VAR(LT)(NUMBER, NUMBER);
BOOLEAN VAR(GT)(NUMBER, NUMBER);
BOOLEAN VAR(LTE)(NUMBER, NUMBER);
BOOLEAN VAR(GTE)(NUMBER, NUMBER);
BOOLEAN VAR(numberQMARK)(ANY);
BOOLEAN VAR(listQMARK)(ANY);
BOOLEAN VAR(stringQMARK)(ANY);
BOOLEAN VAR(blockQMARK)(ANY);
BOOLEAN VAR(booleanQMARK)(ANY);
BOOLEAN VAR(integerQMARK)(ANY);
ANY VAR(first)(LIST);
LIST VAR(rest)(LIST);
STRING VAR(head)(STRING);
STRING VAR(tail)(STRING);
LIST VAR(push)(ANY, LIST);
BOOLEAN VAR(emptyQMARK)(LIST);
LIST VAR(list)(BLOCK);
STRING VAR(join)(NUMBER);
NUMBER VAR(stringDASHlength)(STRING);
STRING VAR(substring)(NUMBER, NUMBER, STRING);
STRING VAR(input)();
STRING VAR(read)(STRING);
NUMBER VAR(number)(STRING);
STRING VAR(path)();
LIST VAR(stack)();
void VAR(write)(STRING, ANY);
LIST VAR(parameters)();
void VAR(stop)();
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
RECORD VAR(record)(BLOCK);
ANY VAR(get)(SYMBOL, RECORD);
BOOLEAN VAR(has)(SYMBOL, RECORD);
