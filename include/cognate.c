#ifndef COGNATE_C
#define COGNATE_C

#include "cognate.h"
#include "types.h"

static const char *stack_start;
static __attribute__((unused)) char if_status = 2;

static void run_program();
static cognate_object check_block(cognate_object);
static void copy_blocks();
static void check_call_stack();
static cognate_block block_copy_gc(cognate_block);
static void block_dealloc_callback(void* _, __attribute__((unused)) void*);

#include "table.c"
#include "stack.c"
#include "io.c"
#include "error.c"
#include "type.c"
#include "func.c"

#include <time.h>
#include <sys/resource.h>
#include <libgen.h>
#include <Block.h>
#include <locale.h>
#ifndef noGC
#include <gc/gc.h>
#endif

static struct rlimit stack_max;

int main(int argc, char** argv)
{
  // Get return stack limit
  char a;
  stack_start = &a;
  if unlikely(getrlimit(RLIMIT_STACK, &stack_max) == -1)
  {
    throw_error("Cannot get return stack limit!");
  }
  // Set locale for strings.
  if unlikely(setlocale(LC_ALL, "") == NULL)
  {
    throw_error("Cannot set locale!");
  }
  // Init GC
#ifndef noGC
  GC_INIT();
#endif
  // Seed the random number generator properly.
  struct timespec ts;
  if unlikely(timespec_get(&ts, TIME_UTC) == 0)
  {
    throw_error("Cannot get system time!");
  }
  srandom(ts.tv_nsec ^ ts.tv_sec); // TODO make random more random.
  // Load parameters
  params.start = (cognate_object*) GC_MALLOC (sizeof(cognate_object) * (argc-1));
  params.top = params.start + argc - 1;
  while (argc --> 1)
  {
    char* str = argv[argc];
    params.start[argc-1] = (cognate_object){.type=string, .string=str};
  }
  // Bind error signals.
  signal(SIGABRT, handle_signal);
  signal(SIGFPE,  handle_signal);
  signal(SIGILL,  handle_signal);
  signal(SIGINT,  handle_signal);
  signal(SIGTERM, handle_signal);
  signal(SIGSEGV, handle_signal); // Will only sometimes work.
  // Generate a stack.
  init_stack();
  // Actually run the program.
  run_program();
  // Clean up.
  if unlikely(stack.items.top != stack.items.start)
  {
    word_name = NULL;
    function_name = NULL;
    throw_error("Program exiting with non-empty stack of length %ti", stack.items.top - stack.items.start);
  }
  GC_gcollect();
}

static cognate_object check_block(cognate_object obj)
{
  if unlikely(obj.type==block) obj.block = block_copy_gc(obj.block);
  return obj;
}

void block_dealloc_callback(void* blk, __attribute__((unused)) void* _)
{
  //printf("Dealloc-ing block %p\n", blk);
  Block_release(blk);
}

static void copy_blocks()
{
  //printf("Copying %zi blocks", stack.uncopied_blocks);
  for (cognate_object* obj = stack.items.top; stack.uncopied_blocks > 0; --obj)
  {
    if unlikely(obj->type==block)
    {
      obj->block = block_copy_gc(obj->block); // Copy block to heap.
      --stack.uncopied_blocks;
    }
  }
}

static cognate_block block_copy_gc(cognate_block blk)
{
  blk = Block_copy(blk); // Copy block to heap.
  //printf("Alloc-ing block %p\n", (void*)blk);
  GC_register_finalizer(blk, &block_dealloc_callback, NULL, NULL, NULL);
  return blk;
}

static void check_call_stack()
{
  // Performance here is not great.
  static unsigned short function_calls = 1024;
  if unlikely(!--function_calls)
  {
    function_calls = 1024;
    static long old_stack_size = 0;
    const char stack_end;
    // if (how much stack left < stack change between checks)
    if unlikely(stack_max.rlim_cur + &stack_end - stack_start < stack_start - &stack_end - old_stack_size)
    {
      throw_error("Call stack overflow - too much recursion! (call stack is %ti bytes)", stack_start - &stack_end);
    }
    old_stack_size = stack_start - &stack_end;
  }
}

#endif
