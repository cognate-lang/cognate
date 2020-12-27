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

#include <stdlib.h>
#include <string.h>
#include <gc/gc.h>

static unsigned long hash(const char *str)
{
  // http://www.cse.yorku.ca/~oz/hash.html
  unsigned long hash = 0;
  int c;
  while ((c = *str++))
    hash = c + (hash << 6) + (hash << 16) - hash;
  return hash;
}


static cognate_table table_add(const unsigned long key_hash, const cognate_object value, cognate_table tab)
{
  // This will replace a key if it is already in the table.
  unsigned long table_size = tab.items.top - tab.items.start;
  unsigned long shrunk_hash = key_hash % table_size;
  for (char tries = 0;; ++tries)
  {
    if (++shrunk_hash == table_size) shrunk_hash = 0; // WILL LOOP INDEFINITELY
    // Add to table is bucket is either empty or has the same confirmation hash (IE same key).
    if (tab.items.start[shrunk_hash].type == NOTHING || key_hash == tab.confirmation_hash[shrunk_hash])
    {
      tab.items.start[shrunk_hash] = value;
      // Confirmation hash should probably prevent collisions.
      tab.confirmation_hash[shrunk_hash] = key_hash;
      return tab;
    }
    else if (tries == MAX_TABLE_TRIES)
    {
      tab = table_grow(tab);
      table_size = tab.items.top - tab.items.start;
      shrunk_hash = key_hash % table_size;
      tries = 0;
    }
  }
}

static cognate_object table_get(const char* const key, const cognate_table tab)
{
  cognate_object obj = table_get_hash(hash(key), tab);
  if (obj.type == NOTHING) throw_error("Cannot find key '%s' in table!", key);
  return obj;
}

static cognate_object table_get_hash(const unsigned long key_hash, const cognate_table tab)
{
  const unsigned long table_size = tab.items.top - tab.items.start;
  unsigned long shrunk_hash = key_hash % table_size;
  for (char tries = 0; tries < MAX_TABLE_TRIES; ++tries)
  {
    if (++shrunk_hash == table_size) shrunk_hash = 0;
    if (key_hash == tab.confirmation_hash[shrunk_hash])
    {
      return tab.items.start[shrunk_hash];
    }
  }
  return (cognate_object){.type=NOTHING};
}

static cognate_table table_grow(const cognate_table tab)
{
  const long table_size = tab.items.top - tab.items.start;
  const long new_table_size = table_size * LIST_GROWTH_FACTOR;
  cognate_table tab2;
  tab2.items.start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * new_table_size);
  tab2.items.top = tab2.items.start + new_table_size;
  // Segfaults with normal cognate_malloc() for some reason.
  tab2.confirmation_hash = (unsigned long*) cognate_malloc_atomic (sizeof(unsigned long) * new_table_size);
  for (int i = 0; i < table_size; ++i)
  {
    if (tab.items.start[i].type != NOTHING)
    {
      tab2 = table_add(tab.confirmation_hash[i], tab.items.start[i], tab2);
    }
  }
  return tab2;
}

static cognate_table table_copy(const cognate_table tab)
{
  // Tables are copy on write.
  // This means performance of Insert function is pretty bad.
  const unsigned long table_size = tab.items.top - tab.items.start;
  cognate_table tab2;
  tab2.items.start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * table_size);
  tab2.items.top = tab2.items.start + table_size;
  tab2.confirmation_hash = (unsigned long*) cognate_malloc (sizeof(unsigned long) * table_size);
  memmove(tab2.items.start, tab.items.start, table_size * sizeof(cognate_object));
  memmove(tab2.confirmation_hash, tab.confirmation_hash, table_size * sizeof(unsigned long));
  return tab2;
}

#endif
