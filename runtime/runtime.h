#pragma once

#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2

#include <stddef.h>
#include <stdio.h>
#include <threads.h>
#include <Block.h>

#define INITIAL_READ_SIZE  64
#define INITIAL_STACK_SIZE 64
#define LIST_GROWTH_FACTOR 1.5
#define STACK_MARGIN_KB    50

typedef unsigned long ANY;
typedef ANY* restrict ANYPTR;
typedef void(^BLOCK)();
typedef _Bool BOOLEAN;
typedef double NUMBER;
typedef const char* restrict STRING;
typedef const struct cognate_list* restrict LIST;
typedef struct cognate_group* restrict GROUP;
typedef const char* restrict SYMBOL;

typedef enum cognate_type
{
  NOTHING = 0,
  boolean = 1,
  string  = 2,
  list    = 3,
  group   = 4,
  block   = 5,
  symbol  = 6,
  number  = -1,
} cognate_type;

typedef struct type_punning
{
  double d;
  long l;
} type_punning;

typedef struct cognate_list
{
  LIST next;
  ANY object;
} cognate_list;

typedef struct cognate_group
{
  size_t len;
  struct
  {
    SYMBOL name;
    ANY object;
  } items [1];
} cognate_group;

typedef struct cognate_stack
{
  ANYPTR start; // Pointer to start.
  ANYPTR top;   // Pointer to top.
  ptrdiff_t size;  // Allocated size of the stack.
  ANY cache;
} cognate_stack;

#define NAN_MASK 0x7ff8000000000000
#define PTR_MASK 0x0000ffffffffffff
#define TYP_MASK 0x0007000000000000
#define NIL_OBJ  NAN_MASK

#define SET_FUNCTION_STACK_START() \
  function_stack_start = __builtin_frame_address(0); \
  function_stack_top = function_stack_start - function_stack_size;

#define VAR(name) ___##name
#define SYM(name) ____##name
#define CALL(name, args) VAR(name) args
#define CALLDEBUG(name, args) (set_word_name(#name), set_line_num(__LINE__), VAR(name) args)

#define PREDEF(name) __block BLOCK VAR(name) = ^{ throw_error("Function '"#name"' called before definition!'"); };

#define SET(name, val) VAR(name) = val;

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)   (__builtin_expect((_Bool)(expr), 1))

// Global variables
thread_local extern cognate_stack stack;
extern LIST cmdline_parameters;
extern const char* restrict word_name;
extern int line_num;

extern thread_local const char* restrict function_stack_top;
extern thread_local const char* restrict function_stack_start;
extern ptrdiff_t function_stack_size;

// Variables and  needed by functions.c defined in runtime.c
void init_stack();
void set_function_stack_start();
void expand_stack();
void print_object(const ANY object, FILE *, const _Bool);
void _Noreturn __attribute__((format(printf, 1, 2))) throw_error_fmt(const char* restrict const, ...);
void _Noreturn throw_error(const char* restrict const);
_Bool compare_objects(ANY, ANY);

void* gc_malloc(size_t);
void gc_collect();
void gc_init();
char* gc_strdup(char*);
char* gc_strndup(char*, size_t);

#define gc_new(t) (t*) gc_malloc (sizeof(t))

// Variables and functions needed by compiled source file defined in runtime.c
cognate_type get_type(ANY);
NUMBER unbox_number(ANY);
ANY box_number(NUMBER);
BOOLEAN unbox_boolean(ANY);
ANY box_boolean(BOOLEAN);
STRING unbox_string(ANY);
ANY box_string(STRING);
LIST unbox_list(ANY);
ANY box_list(LIST);
GROUP unbox_group(ANY);
ANY box_group(GROUP);
SYMBOL unbox_symbol(ANY);
ANY box_symbol(SYMBOL);
BLOCK unbox_block(ANY);
ANY box_block(BLOCK);

void init(int, char **);
void cleanup();
void push(ANY);
ANY pop();
ANY peek();
void flush_stack_cache();
int stack_length();
void check_function_stack_size();
void set_word_name(const char* restrict const);
void set_line_num(int);

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
extern BOOLEAN VAR(true);
extern BOOLEAN VAR(false);
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
GROUP VAR(group)(BLOCK);
ANY VAR(the)(SYMBOL, GROUP);
BOOLEAN VAR(has)(SYMBOL, GROUP);
ANY VAR(index)(NUMBER, LIST);
void VAR(puts)(BLOCK);
void VAR(prints)(BLOCK);
BLOCK VAR(precompute)(BLOCK);
BLOCK VAR(parallelDASHprecompute)(BLOCK);
void VAR(wait)(NUMBER);
