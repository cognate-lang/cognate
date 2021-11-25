#include <setjmp.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include "runtime.h"
#include <limits.h>

#define MAP_SIZE 0x10000000000

#define BITMAP_EMPTY 0x0
#define BITMAP_ALLOC 0x1
#define BITMAP_FREE  0x2

#define BITMAP_CLEAR_ALLOCS 0xAAAAAAAAAAAAAAAA

/*
 * Cognate's Garbage Collector
 *
 * This is a simplistic tracing conservative gc, using a bitmap allocator.
 * It has no dependencies on malloc() or free().
 *
 * TODO:
 *  - Make the bitmap only use 2 bits per long like it should, instead of a byte.
 *  - Track the position of the first free node in the bitmap.
 *  - Use vector intrinsics to speed up bitmap checking/modifying.
 *  - Make valgrind shut up.
 */

static char* bitmap;
static uintptr_t* heap_start;
static uintptr_t* heap_top;
static char* bitmap_search_start;

void gc_set_bitmap(uintptr_t* ptr, char bit)
{
  bitmap[ptr - heap_start] = bit;
}

char gc_get_bitmap(uintptr_t* ptr)
{
  return bitmap[ptr - heap_start];
}

void gc_init()
{
  bitmap = bitmap_search_start = mmap(0, MAP_SIZE/32, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  heap_start = heap_top        = mmap(0, MAP_SIZE,    PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  gc_set_bitmap(heap_start, BITMAP_FREE);
}

void* gc_malloc(size_t bytes)
{
  ptrdiff_t longs = (bytes + 7) / sizeof(uintptr_t);
  char* free_start = bitmap_search_start = memchr(bitmap_search_start, BITMAP_FREE, ULONG_MAX);
  for (char* ptr = bitmap_search_start ;; )
  {
    char* free_end = memchr(free_start, BITMAP_ALLOC, longs);
    if (free_end)
    {
      ptr = free_end;
      free_start = memchr(ptr, BITMAP_FREE, ULONG_MAX);
      continue;
    }
    free_end = free_start + longs;
    *free_end = BITMAP_FREE;
    if (heap_top - heap_start < free_end - bitmap) heap_top = heap_start + (free_end - bitmap);
    *free_start = BITMAP_ALLOC;
    memset(free_start + 1, 0, free_end - free_start - 1);
    return heap_start + (free_start - bitmap);
  }
}

void* gc_realloc(void* src, size_t sz)
{
  void* buf = gc_malloc(sz);
  return memcpy(buf, src, sz);
}

static void collect_root(uintptr_t object)
{
  uintptr_t* ptr = (uintptr_t*)(object & PTR_MASK);
  if ((object & ~PTR_MASK && (object & NAN_MASK) != NAN_MASK)
   || ptr < heap_start || ptr >= heap_top
   || gc_get_bitmap(ptr) != BITMAP_FREE) return;
  gc_set_bitmap(ptr, BITMAP_ALLOC);
  collect_root(ptr[0]);
  // No need to optmize the bitmap addressing, since it's O(n) anyways.
  for (size_t i = 1; !gc_get_bitmap(ptr + i); ++i)
    collect_root(ptr[i]);
}

__attribute__((noinline)) void gc_collect()
{
  for (uintptr_t* p = (uintptr_t*)bitmap; (char*)p < (char*)bitmap + (heap_top - heap_start); ++p)
    *p &= BITMAP_CLEAR_ALLOCS;
  bitmap_search_start = bitmap;
  __attribute__((unused)) volatile cognate_stack s = stack;
  jmp_buf a;
  setjmp(a);
  uintptr_t* sp = (uintptr_t*)&sp;
  for (uintptr_t* root = sp + 1; root < (uintptr_t*)function_stack_start; ++root)
    collect_root(*root);
}

char* gc_strdup(char* src)
{
  size_t len = strlen(src);
  char* dest = gc_malloc(len + 1);
  return memcpy(dest, src, len + 1);
}

char* gc_strndup(char* src, size_t bytes)
{
  size_t len = strlen(src);
  if (len < bytes) bytes = len;
  char* dest = gc_malloc(bytes + 1);
  dest[bytes] = '\0';
  return memcpy(dest, src, bytes);
}
