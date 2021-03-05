#ifndef COGNATE_C
#define COGNATE_C

#include "cognate.h"
#include "types.h"

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
#include <Block.h>
#include <locale.h>
#include <sys/resource.h>
#ifndef NO_GC
#include <gc/gc.h>
#endif

#if __has_include(<Block_private.h>)
#define BLOCK_GC
#endif

#ifdef BLOCK_GC
#include <Block_private.h>
BLOCK_EXPORT void* blk_alloc(const unsigned long size, __attribute__((unused)) const _Bool _, __attribute__((unused)) const _Bool __) { return GC_MALLOC(size); }
BLOCK_EXPORT void blk_setHasRefcount(__attribute__((unused)) const void* _, __attribute__((unused)) const _Bool __) {}
BLOCK_EXPORT void blk_gc_assign_strong(void* src, void** dst) { *dst = src; }
BLOCK_EXPORT void blk_gc_assign_weak(const void* src, void* dst) { *(void**)dst = (void*)src; }
BLOCK_EXPORT void blk_gc_memmove(void* dst, void* src, unsigned long size) { memmove(dst, src, size); }
#endif

static const char* stack_start;
static const char* stack_top;

void init(int argc, char** argv)
{
  // Get return stack limit
  char a;
  stack_start = &a;
  struct rlimit stack_limit;
  if unlikely(getrlimit(RLIMIT_STACK, &stack_limit) == -1)
  {
    throw_error("Cannot get return stack limit!");
  }
  stack_top = stack_start - stack_limit.rlim_cur;
  // Set locale for strings.
  if unlikely(setlocale(LC_ALL, "") == NULL)
  {
    throw_error("Cannot set locale!");
  }
  // Init GC
#ifndef NO_GC
  GC_INIT();
#ifdef BLOCK_GC
  _Block_use_GC(blk_alloc, blk_setHasRefcount, blk_gc_assign_strong, blk_gc_assign_weak, blk_gc_memmove);
#else
  #pragma message "Cannot find header <Block_private.h>. Blocks cannot use garbage collection and may leak memory!"
#endif
#else
  #pragma message "Compiling without the garbage collector will cause memory leaks!"
#endif
  // Seed the random number generator properly.
  struct timespec ts;
  if unlikely(timespec_get(&ts, TIME_UTC) == 0)
  {
    throw_error("Cannot get system time!");
  }
  srandom(ts.tv_nsec ^ ts.tv_sec); // TODO make random more random.
  // Load parameters
  while (argc --> 1)
  {
    cognate_list_node* tmp = GC_NEW (cognate_list_node);
    tmp->object = OBJ(string, argv[argc]);
    tmp->next = params;
    params = tmp;
  }
  // Bind error signals.
  char signals[] = {SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGFPE, SIGSEGV, SIGPIPE, SIGTERM, SIGCHLD};
  for (size_t i = 0; i < sizeof(signals); ++i) signal(signals[i], handle_signal);
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
  return unlikely(obj.type == block) ? OBJ(heap_block, Block_copy(obj.block)) : obj;
}

static void copy_blocks()
{
  //printf("Copying %li blocks\n", stack.uncopied_blocks);
  for (cognate_object* obj = stack.top - 1; stack.uncopied_blocks; --obj)
  {
    if unlikely(obj->type == block)
    {
      *obj = OBJ(heap_block, Block_copy(obj->block));
      --stack.uncopied_blocks;
    }
  }
}

static void check_call_stack()
{
  char sp;
  if unlikely(&sp - stack_top < STACK_MARGIN_KB * 1024)
    throw_error("Call stack overflow - too much recursion! (call stack is %tikB out of maximum %tikB)", (stack_start - &sp) >> 10, (stack_start - stack_top) >> 10);
}

#endif
