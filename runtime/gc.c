#include <setjmp.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <limits.h>
#include "runtime.h"

#define PAGE_SIZE 4096

#define MAP_SIZE 0x10000000000

#define BITMAP_EMPTY 0x0
#define BITMAP_FREE  0x1
#define BITMAP_ALLOC 0x3

#define BITMAP_INDEX(ptr) bitmap[ptr - heap_start]

/*
 * Cognate's Garbage Collector
 *
 * This is a simplistic tracing conservative gc, using a bitmap allocator.
 * It has no dependencies on malloc() or free().
 * Well under 100 SLOC is also ridiculously small for a garbage collector.
 *
 * TODO:
 *  - Remove consequetive GC_FREEs after doing gc as then we don't have to
 *    memset the bitmap when we allocate.
 *  - Use a free list / free stack for allocating as this will improve
 *    performance
 *  - Make the bitmap only use 2 bits per long like it should, instead of a byte.
 *  - Use vector intrinsics to speed up bitmap checking/modifying.
 *
 * FIXME:
 *  - Doesn't work under valgrind.
 *  - An object will be deallocated if only referenced from other threads(!)
 *    > Collector either needs to iterate over parent threads' stacks, or a
 *    > thread-global collector iterates over all threads stacks. Second
 *    > method seems simpler and uses less memory but would also require
 *    > locks on gc_malloc().
 */

thread_local static uintptr_t* restrict heap_start;
thread_local static uintptr_t* restrict heap_top;

thread_local static uint8_t* restrict bitmap;
thread_local static uint8_t* restrict free_start;

void gc_init(void)
{
  bitmap = free_start   = mmap(0, MAP_SIZE/8, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  heap_start = heap_top = mmap(0, MAP_SIZE,   PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  if (heap_start == MAP_FAILED || bitmap == MAP_FAILED)
    throw_error("memory map failure - are you trying to use valgrind?");
  BITMAP_INDEX(heap_start) = BITMAP_FREE;
}

static void __attribute__((unused)) show_heap_usage(void)
{
  printf("%p -> %p\n", (void*)heap_start, (void*)heap_top);
  for (uintptr_t* i = heap_start; i < heap_top; ++i)
  {
    switch(BITMAP_INDEX(i))
    {
      case BITMAP_ALLOC: putc('#', stdout); break;
      case BITMAP_FREE:  putc('-', stdout); break;
      default:           putc('?', stdout);
    }
  }
  putc('\n', stdout);
}

__attribute__((malloc, hot, assume_aligned(sizeof(uint64_t)), alloc_size(1), returns_nonnull))
void* gc_malloc(size_t bytes)
{
  thread_local static int byte_count = 0;
  byte_count += bytes;
  if unlikely(byte_count > 1024 * 1024) gc_collect(), byte_count = 0;
  const size_t longs = (bytes + 7) / sizeof(uintptr_t);
  free_start = memchr(free_start, BITMAP_FREE, LONG_MAX);
  for (uint8_t* restrict free_end; unlikely(free_end = memchr(free_start + 1, BITMAP_ALLOC, longs - 1)); )
    free_start = memchr(free_end + 1, BITMAP_FREE, LONG_MAX);
  memset(free_start + 1, BITMAP_EMPTY, longs - 1);
  uint8_t* restrict free_end = free_start + longs;
  uintptr_t* buf = heap_start + (free_start - bitmap);
  *free_start = BITMAP_ALLOC;
  if unlikely(heap_top < buf + longs) heap_top = buf + longs;
  if (*free_end == BITMAP_EMPTY) *free_end = BITMAP_FREE;
  free_start = free_end;
  return buf;
}

static void gc_collect_root(uintptr_t object)
{
  uintptr_t* restrict ptr = (uintptr_t*)(object & PTR_MASK);
  if likely((object != (uintptr_t)ptr && !is_nan(object))
   || ptr < heap_start || ptr >= heap_top
   || BITMAP_INDEX(ptr) != BITMAP_FREE) return;
  /* Cognate does not use pointers to the middle of gc objects,
   * but if in future it does, then the garbage collector will
   * need to iterate back through the bitmap if ptr is empty. */
  BITMAP_INDEX(ptr) = BITMAP_ALLOC;
  // No need to optimize the bitmap addressing, since it's O(n) anyways.
  for (uintptr_t* p = ptr + 1; BITMAP_INDEX(p) == BITMAP_EMPTY; ++p)
    gc_collect_root(*p);
  gc_collect_root(*ptr);
}

__attribute__((noinline)) void gc_collect(void)
{
  for (uintptr_t* restrict p = (uintptr_t*)bitmap;
      (uint8_t*)p < bitmap + (heap_top - heap_start); ++p)
    *p &= 0x5555555555555555;
  jmp_buf a;
  setjmp(a);
  volatile ANY s = (ANY)stack.start;
  uintptr_t* sp = (uintptr_t*)&s;
  for (uintptr_t* root = sp; root <= (uintptr_t*)function_stack_start; ++root)
    gc_collect_root(*root);
  free_start = bitmap;
}

char* gc_strdup(char* src)
{
  const size_t len = strlen(src);
  return memcpy(gc_malloc(len + 1), src, len + 1);
}

char* gc_strndup(char* src, size_t bytes)
{
  const size_t len = strlen(src);
  if (len < bytes) bytes = len;
  char* dest = gc_malloc(bytes + 1);
  dest[bytes] = '\0';
  return memcpy(dest, src, bytes);
}
