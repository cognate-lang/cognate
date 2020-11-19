#ifndef COGNATE_H
#define COGNATE_H

#define TABLE_GROWTH_FACTOR 1.5
#define MAX_TABLE_TRIES 3
#define PATH_MAX 4096
#define INITIAL_INPUT_SIZE 64
#define PATH_MAX 4096
#define MIN_TABLE_SIZE 2

static char exe_path[PATH_MAX+1];
static char *exe_dir;
static char *exe_name;

#define program(body) \
  int main(int argc, char **argv) \
  { \
    init(argc, argv); \
    body \
    cleanup(); \
    return 0; \
  }

#define mutable   __block
#define immutable const

#define copy   1
#define nocopy 0

// Global-local variable swapping is causing performance losses. :(
#define function(name, flags, docopy, body) \
  flags cognate_block cognate_function_ ## name = make_block(docopy, \
  { \
    char* temp_func_name = function_name; \
    function_name = #name; \
    check_call_stack(); \
    body \
    function_name = temp_func_name; \
  });

#ifdef debug
  #define call(name) {debug_printf("Calling %s\n", #name); cognate_function_ ## name();}
#else 
  #define call(name) {cognate_function_ ## name();}
#endif

// Macro to define external cognate function.
#define external_function(name, body) \
  static void cognate_function_ ## name () body

// Macro for defining external cognate variables with specified type.
#define external_variable(name, type, value) \
  static void cognate_function_ ## name () { \
    push(type, value); }

/*
#define mutate_function(name, docopy, body) \
  cognate_function_ ## name = make_block(docopy, body);
*/

// Internal cognate variable.
#define variable(name, flags) \
  immutable cognate_object cognate_variable_ ## name = pop_any(); \
  flags cognate_block cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

// Mutate internal variable.
#define mutate_variable(name) \
  immutable cognate_object cognate_variable_ ## name = check_block(pop_any()); /* Can't remember what check_block does here */\
  cognate_function_##name = Block_copy(^{push_any(cognate_variable_ ## name);});
 
#define make_block(docopy, body) \
  ^{ \
    /* Temp variable causes ~10% performance loss :( */\
    const ptrdiff_t temp_modified = stack.modified - stack.items.start; \
    stack.modified = stack.items.top; \
    body \
    if (docopy) copy_blocks(); \
    stack.modified = temp_modified + stack.items.start; \
  }

#ifndef noGC
  #define malloc  GC_MALLOC
  #define realloc GC_REALLOC
  #define malloc_atomic GC_MALLOC_ATOMIC
#else
  #define malloc_atomic malloc
#endif

#ifdef debug
#define debug_printf(str, ...) \
  fprintf(stderr, "[DEBUG] %s:%d -> "#str"\n", __FILE__, __LINE__, __VA_ARGS__);
#else
#define debug_printf(str, ...)
#endif

#define throw_error_fmt(fmtstr, ...) \
{ \
  char str[strlen(fmtstr) + sizeof(__VA_ARGS__)]; \
  sprintf(str, fmtstr, __VA_ARGS__); \
  throw_error(str); \
}

#define unlikely(expr) __builtin_expect((_Bool)(expr), 0)
#define likely(expr)   __builtin_expect((_Bool)(expr), 1)

#endif
