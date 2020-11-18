#ifndef COGNATE_C
#define COGNATE_C

#define mutable   __block
#define immutable const

#define copy   1
#define nocopy 0

#ifndef noGC
  #define malloc  GC_MALLOC
  #define realloc GC_REALLOC
  #define malloc_atomic GC_MALLOC_ATOMIC
#else
  #define malloc_atomic malloc
#endif

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


/*
#define mutate_function(name, docopy, body) \
  cognate_function_ ## name = make_block(docopy, body);
*/

#define program(body) \
  int main(int argc, char **argv) \
  { \
    init(argc, argv); \
    body \
    cleanup(); \
    return 0; \
  }

#define variable(name, flags) \
  immutable cognate_object cognate_variable_ ## name = pop_any(); \
  flags cognate_block cognate_function_ ## name = ^{push_any(cognate_variable_ ## name);};

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

#define unlikely(expr) __builtin_expect((_Bool)(expr), 0)
#define likely(expr)   __builtin_expect((_Bool)(expr), 1)

#define PATH_MAX 4096
static char exe_path[PATH_MAX+1];
static char *exe_dir;
static char *exe_name;

#include <gc.h>
#include <time.h>
#include "stack.c"
#include "io.c"
#include "error.c"
#include "type.c"
#include <sys/resource.h>
#include <unistd.h>
#include "func.c"
#include <libgen.h>

ssize_t readlink(const char *pathname, char *buf, size_t bufsiz);

static char *stack_start;
static struct rlimit stack_max;

static void get_params(int argc, char** argv)
{
  params.start = (cognate_object*) malloc (sizeof(cognate_object) * (argc-1));
  params.top = params.start + argc - 1;
  while (argc-- >= 1)
  {
    char* str = argv[argc];
    params.start[argc-1] = (cognate_object){.type=string, .string=str};
  }

}


static void init(int argc, char** argv)
{
  // Get return stack limit
  char a;
  stack_start = &a;
  getrlimit(RLIMIT_STACK, &stack_max);
  // Get executable path stuff.
  readlink("/proc/self/exe", exe_path, PATH_MAX);
  exe_name = basename(exe_path);
  exe_dir  = dirname(exe_path);
  // Seed the random number generator properly.
#ifndef noGC
  GC_INIT(); // Portability.
#endif
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  srand(ts.tv_nsec ^ ts.tv_sec);
  // Generate a stack.
  init_stack();
  get_params(argc, argv);
}

static void cleanup()
{
  if (unlikely(stack.items.top != stack.items.start))
  {
    char err[58];
    sprintf(err, "Program exiting with non-empty stack of length %lu", stack.items.top - stack.items.start);
    throw_error(err);
  }
}

static cognate_object check_block(cognate_object obj)
{
  (obj.type==block) && (obj.block = Block_copy(obj.block));
  return obj;
}


static void copy_blocks()
{
  for (; stack.modified != stack.items.top; stack.modified++)
  {
    (stack.modified->type==block) && (stack.modified->block = Block_copy(stack.modified->block)); // Copy block to heap.
  }
}

static void check_call_stack()
{
  // Performance here is not great.
  static unsigned short calls = 0;
  ++calls;
  if (unlikely(calls > 1024))
  {
    calls = 0;
    static long old_stack_size;
    char b;
    // if (how much stack left < stack change between checks)
    if ((long)stack_max.rlim_cur + (&b - stack_start) < stack_start - &b - old_stack_size)
    {
      throw_error("Call stack overflow! Too much recursion!");
    }
    old_stack_size = stack_start - &b;
  }
}

#endif
