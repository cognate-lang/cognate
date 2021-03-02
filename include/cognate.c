#ifndef COGNATE_C
#define COGNATE_C

#include "cognate.h"
#include "types.h"

static void init(int argc, char** argv);
static void cleanup();
static cognate_object check_block(cognate_object);
static void copy_blocks();

#include "table.c"
#include "stack.c"
#include "io.c"
#include "error.c"
#include "type.c"
#include "func.c"

#include <sys/resource.h>
#include <time.h>
#include <libgen.h>
#include <Block.h>
#include <locale.h>
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

static char sig_stack[SIGSTKSZ * 2];

void init(int argc, char** argv)
{
  // Get return stack limit
#ifdef CALL_STACK_KB
  struct rlimit stack_max;
  stack_max.rlim_cur = CALL_STACK_KB << 10;
  setrlimit(RLIMIT_STACK, &stack_max);
#endif
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
  stack_t sig_stk = {.ss_sp=sig_stack, .ss_size=SIGSTKSZ * 2};
  struct sigaction action = {.sa_handler=handle_signal, .sa_flags=SA_ONSTACK};
  sigaltstack(&sig_stk, NULL);
  sigemptyset(&action.sa_mask);
  sigaction(SIGABRT, &action, NULL);
  sigaction(SIGFPE,  &action, NULL);
  sigaction(SIGILL,  &action, NULL);
  sigaction(SIGINT,  &action, NULL);
  sigaction(SIGTERM, &action, NULL);
  sigaction(SIGSEGV, &action, NULL);
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
  if unlikely(obj.type == block) obj = OBJ(heap_block, Block_copy(obj.block));
  return obj;
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

#endif
