#include <setjmp.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include "runtime.h"

#define MAP_SIZE 0x10000000000

#define BITMAP_EMPTY 0
#define BITMAP_ALLOC 1
#define BITMAP_FREE  2

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

static char* bitmap = NULL;
static uintptr_t* heap_start;
static uintptr_t* heap_top;

void gc_set_bitmap(uintptr_t* ptr, char bit)
{
  bitmap[(ptr - heap_start) % MAP_SIZE] = bit;
}

void gc_init()
{
  bitmap =                mmap(0, MAP_SIZE/32, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  heap_start = heap_top = mmap(0, MAP_SIZE,    PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  gc_set_bitmap(heap_start, BITMAP_FREE);
}

char gc_get_bitmap(uintptr_t* ptr)
{
  return bitmap[(ptr - heap_start) % MAP_SIZE];
}

void* gc_malloc(size_t bytes)
{
  size_t longs = (bytes + 7) / sizeof(uintptr_t);
  size_t slots = 0;
  for (uintptr_t* p = (uintptr_t*)heap_start ;; ++p)
  {
    // TODO bitwise vector bitmap stuff here would be really cool.
    // THIS is the performance bottleneck.
    char map = gc_get_bitmap(p);
    if (!slots && map == BITMAP_FREE) slots = 1;
    else if (map == BITMAP_ALLOC) slots = 0;
    else if (slots && map != BITMAP_ALLOC) slots++;
    if (slots == longs)
    {
      uintptr_t* buf = p - slots + 1;
      uintptr_t* next = p + 1;
      if (!gc_get_bitmap(p + 1)) gc_set_bitmap(next, BITMAP_FREE);
      if (heap_top < p + 1) heap_top = next;
      gc_set_bitmap(buf, BITMAP_ALLOC);
      for (uintptr_t* q = buf + 1; q < next; ++q)
        gc_set_bitmap(q, BITMAP_EMPTY);
      return buf;
    }
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
  // Funky bitmap operation to determine number of children??? TODO
  for (size_t i = 1; !gc_get_bitmap(ptr + i); ++i)
    collect_root(ptr[i]);
}

__attribute__((noinline)) void gc_collect()
{
  // TODO clearing the allocated bits can be done much easier using bitwise AND over longs.
  for (uintptr_t* p = (uintptr_t*)heap_start; p < heap_top; ++p)
    if (gc_get_bitmap(p) == BITMAP_ALLOC)
      gc_set_bitmap(p, BITMAP_FREE);
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
