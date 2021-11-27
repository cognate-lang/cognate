#include <setjmp.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <limits.h>
#include "runtime.h"

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
 *  - Make the bitmap only use 2 bits per long like it should, instead of a byte.
 *  - Use vector intrinsics to speed up bitmap checking/modifying.
 *  - Make valgrind shut up.
 */

static uintptr_t* restrict heap_start;
static uintptr_t* restrict heap_top;

static uint8_t* restrict bitmap;
static uint8_t* restrict free_start;

void gc_init()
{
  bitmap = free_start   = mmap(0, MAP_SIZE/32, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  heap_start = heap_top = mmap(0, MAP_SIZE,    PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  BITMAP_INDEX(heap_start) = BITMAP_FREE;
}

void* gc_malloc(size_t bytes)
{
  static int byte_count = 0;
  byte_count += bytes;
  if unlikely(byte_count > 1024 * 1024) gc_collect(), byte_count = 0;
  const size_t longs = (bytes + 7) / sizeof(uintptr_t);
  for (uint8_t* restrict free_end; unlikely(free_end = memchr(free_start, BITMAP_ALLOC, longs)); )
    free_start = memchr(free_end, BITMAP_FREE, LONG_MAX);
  uint8_t* restrict free_end = free_start + longs;
  uintptr_t* buf = heap_start + (free_start - bitmap);
  *free_end   = BITMAP_FREE;
  *free_start = BITMAP_ALLOC;
  memset(free_start + 1, BITMAP_EMPTY, longs - 1);
  if unlikely(heap_top < buf)
    heap_top = buf;
  free_start = free_end;
  return buf;
}

void* gc_realloc(void* restrict src, size_t sz)
{
  void* buf = gc_malloc(sz);
  return memcpy(buf, src, sz);
}

static void gc_collect_root(uintptr_t object)
{
  uintptr_t* restrict ptr = (uintptr_t*)(object & PTR_MASK);
  if ((object & ~PTR_MASK && (object & NAN_MASK) != NAN_MASK)
   || ptr < heap_start || ptr >= heap_top
   || BITMAP_INDEX(ptr) != BITMAP_FREE) return;
  BITMAP_INDEX(ptr) = BITMAP_ALLOC;
  // No need to optimize the bitmap addressing, since it's O(n) anyways.
  for (ptrdiff_t i = 1; BITMAP_INDEX(ptr + i) == BITMAP_EMPTY; ++i)
    gc_collect_root(ptr[i]);
  gc_collect_root(ptr[0]);
}

__attribute__((noinline)) void gc_collect()
{
  for (uintptr_t* restrict p = (uintptr_t*)bitmap; (uint8_t*)p < bitmap + (heap_top - heap_start); ++p)
    *p &= 0x5555555555555555;
  __attribute__((unused)) volatile cognate_stack s = stack;
  jmp_buf a;
  setjmp(a);
  uintptr_t* sp = (uintptr_t*)&sp;
  for (uintptr_t* root = sp + 1; root < (uintptr_t*)function_stack_start; ++root)
    gc_collect_root(*root);
  free_start = memchr(bitmap, BITMAP_FREE, LONG_MAX);
}

char* gc_strdup(char* src)
{
  const size_t len = strlen(src);
  char* dest = gc_malloc(len + 1);
  return memcpy(dest, src, len + 1);
}

char* gc_strndup(char* src, size_t bytes)
{
  const size_t len = strlen(src);
  if (len < bytes) bytes = len;
  char* dest = gc_malloc(bytes + 1);
  dest[bytes] = '\0';
  return memcpy(dest, src, bytes);
}
