#ifndef COGNATE_C
#define COGNATE_C

#include "cognate.h"
#include "types.h"

static const char *stack_start;
static __attribute__((unused)) char if_status = 2;

static void init(int argc, char** argv);
static void cleanup();
static cognate_object check_block(cognate_object);
static void copy_blocks();
static void check_call_stack();

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

#ifdef blockGC
#include <Block_private.h>
BLOCK_EXPORT void* blk_alloc(const unsigned long size, __attribute__((unused)) const _Bool _, __attribute__((unused)) const _Bool __) { return GC_MALLOC(size); }
BLOCK_EXPORT void blk_setHasRefcount(__attribute__((unused)) const void* _, __attribute__((unused)) const _Bool __) {}
BLOCK_EXPORT void blk_gc_assign_strong(void* src, void** dst) { *dst = src; }
BLOCK_EXPORT void blk_gc_assign_weak(const void* src, void* dst) { *(void**)dst = (void*)src; }
BLOCK_EXPORT void blk_gc_memmove(void* dst, void* src, unsigned long size) { memmove(dst, src, size); }
#endif

static struct rlimit stack_max;

void init(int argc, char** argv)
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
#ifdef blockGC
  _Block_use_GC(blk_alloc, blk_setHasRefcount, blk_gc_assign_strong, blk_gc_assign_weak, blk_gc_memmove);
#endif
#endif
  // Seed the random number generator properly.
  struct timespec ts;
  if unlikely(timespec_get(&ts, TIME_UTC) == 0)
  {
    throw_error("Cannot get system time!");
  }
  srandom(ts.tv_nsec ^ ts.tv_sec); // TODO make random more random.
  // Load parameters
  params = NULL;
  while (argc --> 1)
  {
    cognate_list* tmp = GC_NEW (cognate_list);
    tmp->object = (cognate_object){.type=string, .string=argv[argc]};
    tmp->next = params;
    params = tmp;
  }
  // Bind error signals.
  signal(SIGABRT, handle_signal);
  signal(SIGFPE,  handle_signal);
  signal(SIGILL,  handle_signal);
  signal(SIGINT,  handle_signal);
  signal(SIGTERM, handle_signal);
  signal(SIGSEGV, handle_signal); // Will only sometimes work.
  // Initialize the stack.
  init_stack();
}

static void cleanup()
{
  if unlikely(stack.top != stack.start)
  {
    word_name = NULL;
    throw_error("Program exiting with non-empty stack of length %ti", stack.top - stack.start);
  }
}

static cognate_object check_block(cognate_object obj)
{
  if unlikely(obj.type == block) obj.block = Block_copy(obj.block), obj.type = heap_block;
  return obj;
}

static void copy_blocks()
{
  //printf("Copying %li blocks\n", stack.uncopied_blocks);
  for (cognate_object* obj = stack.top - 1; stack.uncopied_blocks; --obj)
  {
    if unlikely(obj->type == block)
    {
      *obj = (cognate_object) {.type=heap_block, .block=Block_copy(obj->block)};
      --stack.uncopied_blocks;
    }
  }
}

static void check_call_stack()
{
  // Performance here is not great.
  static unsigned short function_calls = 1024;
  if unlikely(!--function_calls)
  {
    function_calls = 1024;
    static ptrdiff_t old_stack_size = 0;
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
