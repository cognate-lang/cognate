#ifndef COGNATE_C
#define COGNATE_C

#include "cognate.h"
#include "stack.c"
#include "io.c"
#include "error.c"
#include "type.c"
#include "func.c"
#include <time.h>
#include <sys/resource.h>
#include <libgen.h>
#include <Block.h>

ssize_t readlink(const char *pathname, char *buf, size_t bufsiz);

static char *stack_start;
static struct rlimit stack_max;

static void get_params(int argc, char** argv)
{
  params.start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * (argc-1));
  params.top = params.start + argc - 1;
  while (--argc >= 1)
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
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  srand(ts.tv_nsec ^ ts.tv_sec);
  // Generate a stack.
  init_stack();
  get_params(argc, argv);
#ifndef noGC
  GC_INIT(); // Portability.
#endif
}

static void cleanup()
{
  if (unlikely(stack.items.top != stack.items.start))
  {
    char err[58];
    throw_error_fmt("Program exiting with non-empty stack of length %lu", stack.items.top - stack.items.start);
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
  static unsigned short calls = 1024;
  if (unlikely(!--calls))
  {
    calls = 1024;
    static long old_stack_size;
    char b;
    // if (how much stack left < stack change between checks)
    if (stack_max.rlim_cur + &b - stack_start < stack_start - &b - old_stack_size)
    {
      throw_error_fmt("Call stack overflow! Too much recursion! (call stack is %lu bytes)", stack_start - &b);
    }
    old_stack_size = stack_start - &b;
  }
}

#endif
