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

static char* restrict bitmap;
static uint64_t* restrict heap_start;
static uint64_t* restrict heap_top;
static char* restrict bitmap_search_start;

void gc_set_bitmap(uint64_t* restrict ptr, char bit)
{
  bitmap[ptr - heap_start] = bit;
}

char gc_get_bitmap(uint64_t* restrict ptr)
{
  return bitmap[ptr - heap_start];
}

void gc_init()
{
  bitmap = bitmap_search_start = mmap(0, MAP_SIZE/32, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  heap_start = heap_top        = mmap(0, MAP_SIZE,    PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  gc_set_bitmap(heap_start, BITMAP_FREE);
}

void print_bitmap()
{
  for (uint64_t* restrict i = heap_start; i < heap_top + 5; ++i)
  {
    printf("%x ", gc_get_bitmap(i));
  }
  puts("");
}

void* gc_malloc(size_t bytes)
{
  static int byte_count = 0;
  byte_count += bytes;
  if unlikely(byte_count > 1024 * 1024 * 128)
  {
    gc_collect();
    byte_count = 0;
  }
  const size_t longs = (bytes + 7) / sizeof(uint64_t);
  char* restrict free_start = bitmap_search_start = memchr(bitmap_search_start, BITMAP_FREE, ULONG_MAX);
  for (char* free_end; (free_end = memchr(free_start, BITMAP_ALLOC, longs));)
    free_start = memchr(free_end, BITMAP_FREE, ULONG_MAX);
  char* free_end = free_start + longs;
  *free_end   = BITMAP_FREE;
  *free_start = BITMAP_ALLOC;
  memset(free_start + 1, BITMAP_EMPTY, free_end - free_start - 1);
  if unlikely(heap_top - heap_start < free_end - bitmap)
    heap_top = heap_start + (free_end - bitmap);
  return heap_start + (free_start - bitmap);
}

void* gc_realloc(void* restrict src, size_t sz)
{
  void* buf = gc_malloc(sz);
  return memcpy(buf, src, sz);
}

static void collect_root(uint64_t object)
{
  uint64_t* restrict ptr = (uint64_t*)(object & PTR_MASK);
  if ((object & ~PTR_MASK && (object & NAN_MASK) != NAN_MASK)
   || ptr < heap_start || ptr >= heap_top
   || gc_get_bitmap(ptr) != BITMAP_FREE) return;
  gc_set_bitmap(ptr, BITMAP_ALLOC);
  // No need to optmize the bitmap addressing, since it's O(n) anyways.
  for (ptrdiff_t i = 1; !gc_get_bitmap(ptr + i); ++i)
    collect_root(ptr[i]);
  collect_root(ptr[0]);
}

__attribute__((noinline)) void gc_collect()
{
  printf("collect heap of size %zi\n", heap_top - heap_start);
  for (uint64_t* restrict p = heap_start; p < heap_top; ++p)
    if (gc_get_bitmap(p) == BITMAP_ALLOC) gc_set_bitmap(p, BITMAP_FREE);
  bitmap_search_start = bitmap;
  __attribute__((unused)) volatile cognate_stack s = stack;
  jmp_buf a;
  setjmp(a);
  uint64_t* sp = (uint64_t*)&sp;
  for (uint64_t* root = sp + 1; root < (uint64_t*)function_stack_start; ++root)
    collect_root(*root);
  puts("done");
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
