#ifndef table_c
#define table_c

#include "cognate.h"
#include "types.h"

static unsigned long hash(const char*);
static cognate_table table_add(const unsigned long, const cognate_object, cognate_table);
static cognate_object table_get(const char* const, const cognate_table);
static cognate_object table_get_hash(const unsigned long, const cognate_table);
static cognate_table table_grow(const cognate_table);
static cognate_table table_copy(const cognate_table);

#include "error.c"
#include "type.c"

#include <stdlib.h>
#include <string.h>
#ifndef noGC
#include <gc/gc.h>
#endif

static unsigned long hash(const char *str)
{
  // http://www.cse.yorku.ca/~oz/hash.html
  unsigned long hash = 0;
  int c;
  while ((c = *str++))
    hash = c + (hash << 6) + (hash << 16) - hash;
  return hash;
}

#endif
