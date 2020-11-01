#ifndef TYPE_C
#define TYPE_C

#include <stdio.h>
#include "error.c"
#include "Block.h"
#include <stddef.h>
#include <ctype.h>

#define TABLE_GROWTH_FACTOR 1.5
#define MAX_TABLE_TRIES 3

typedef enum 
{
  // NOTHING is currently only used for unused hashtable bucksts.
  NOTHING=0, // Must be zero because of calloc()
  block, 
  boolean, 
  string, 
  number, 
  list,
  table,
} cognate_type;

typedef void(^cognate_block)();

static int next_type = list + 1;

struct cognate_list
{
  struct cognate_object *start,
                        *top;
};


struct cognate_table
{
  struct cognate_list items;
  long unsigned int *confirmation_hash;
};

typedef struct cognate_table cognate_table;



// __attribute__((packed)) could save memory here.
struct cognate_object
{
  union
  {
    char* string;                  // 64bit pointer
    _Bool boolean;                 // 1bit  bool
    cognate_block block;           // 64bit pointer
    double number;                 // 64bit float
    struct cognate_list  *list;    // 64bit pointer
    struct cognate_table *table;   // 64bit pointer
  };
  cognate_type type : 16;
};

typedef struct cognate_object cognate_object;
typedef struct cognate_list   cognate_list;

static cognate_object check_type(cognate_type, cognate_object);
static const char* lookup_type(cognate_type);
static _Bool compare_objects(cognate_object, cognate_object);
static _Bool compare_lists(cognate_list, cognate_list);
static cognate_table table_grow(cognate_table);

static cognate_object check_type(cognate_type expected_type, cognate_object object)
{
  if (likely(object.type == expected_type)) 
  {
    return object;
  }
  type_error(lookup_type(expected_type), lookup_type(object.type));
}

static const char* lookup_type(cognate_type type)
{
  switch(type)
  {
    case boolean: return "Boolean";
    case string : return "String";
    case block  : return "Block";
    case number : return "Number";
    case list   : return "List";
    case table  : return "Table";
    default:;
  }
  static char type_number[24]; // Max 32 bit type will take up 10 digits.
  sprintf(type_number, "<%i>", type);
  return type_number;
}

static _Bool compare_lists(cognate_list lst1, cognate_list lst2)
{
  ptrdiff_t len = lst1.top - lst1.start;
  if (len != lst2.top - lst2.start) return 0; // Not equal if differing length.
  while (len > 0)
  {
    --len;
    if (!compare_objects(lst1.start[len], lst2.start[len])) // Compare each list object.
    {
      return 0;
    }
  }
  return 1;
}

static _Bool compare_objects(cognate_object ob1, cognate_object ob2)
{
  if (ob1.type != ob2.type)
  {
    return 0; // Not equal if differing types. None of this javascript rubbish.
  }
  switch (ob1.type)
  {
    case number:  return ob1.number  == ob2.number;           break;
    case boolean: return ob1.boolean == ob2.boolean;          break;
    case block:   return ob1.block   == ob2.block;            break;
    case string:  return strcmp(ob1.string, ob2.string) == 0; break;
    case list:    return compare_lists(*ob1.list, *ob2.list); break;
    case table:   return compare_lists(ob1.table->items, ob2.table->items); break;
    default:      return 0;
    // Records are a lie.
  }

  return 0;
}

unsigned int hash(const char *str)
{
  // http://www.cse.yorku.ca/~oz/hash.html
  unsigned long hash = 0;
  int c;
  while ((c = *str++))
    hash = c + (hash << 6) + (hash << 16) - hash;
  return hash;
}


static cognate_table table_add(unsigned long key_hash, cognate_object value, cognate_table tab)
{
  // TODO: find out if this actually works.
  unsigned long table_size = tab.items.top - tab.items.start;
  // WARNING: resizing the table will mess up hashes!!!
  unsigned long shrunk_hash = key_hash % table_size;
  for (char tries = 0;; ++tries)
  {
    if (++shrunk_hash == table_size) shrunk_hash = 0; // WILL LOOP INDEFINITELY
    // Add to table is bucket is either empty or has the same confirmation hash (IE same key).
    if (tab.items.start[shrunk_hash].type == NOTHING || key_hash == tab.confirmation_hash[shrunk_hash])
    {
      tab.items.start[shrunk_hash] = value;
      // Confirmation hash should prevent collisions.
      tab.confirmation_hash[shrunk_hash] = key_hash;
      return tab;
    }
    if (tries == MAX_TABLE_TRIES)
    {
      tab = table_grow(tab);
      table_size = tab.items.top - tab.items.start;
      shrunk_hash = key_hash % table_size;
      tries = 0;
    }
  }
}

static cognate_object table_get(char* key, cognate_table tab)
{
  // TODO: find out if this actually works.
  const unsigned long table_size = tab.items.top - tab.items.start;
  // WARNING: resizing the table will mess up hashes!!!
  const unsigned long key_hash  = hash(key);
  unsigned long shrunk_hash = key_hash % table_size;
  for (char tries = 0; tries < MAX_TABLE_TRIES; ++tries)
  {
    if (++shrunk_hash == table_size) shrunk_hash = 0; // WILL LOOP INDEFINITELY
    if (key_hash == tab.confirmation_hash[shrunk_hash])
    {
      return tab.items.start[shrunk_hash];
    }
  }
  throw_error("Cannot find key in table!");
}

static cognate_table table_grow(cognate_table tab)
{
  const long table_size = tab.items.top - tab.items.start;
  const long new_table_size = table_size * TABLE_GROWTH_FACTOR;
  cognate_table tab2;
  tab2.items.start = (cognate_object*) malloc (sizeof(cognate_object) * new_table_size);
  tab2.items.top = tab2.items.start + new_table_size;
  tab2.confirmation_hash = (unsigned long*) malloc_atomic (sizeof(unsigned long) * new_table_size);
  // Do stuff here.
  for (int i = 0; i < table_size; ++i)
  {
    if (tab.items.start[i].type != NOTHING)
    {
      tab2 = table_add(tab.confirmation_hash[i], tab.items.start[i], tab2);
    }
  }
  return tab2;
}

cognate_table table_copy(cognate_table tab)
{
  const unsigned long table_size = tab.items.top - tab.items.start;
  cognate_table tab2;
  tab2.items.start = (cognate_object*) malloc (sizeof(cognate_object) * table_size);
  tab2.items.top = tab2.items.start + table_size;
  tab2.confirmation_hash = (unsigned long*) malloc (sizeof(unsigned long) * table_size);
  memcpy(tab2.items.start, tab.items.start, table_size * sizeof(cognate_object));
  memcpy(tab2.confirmation_hash, tab.confirmation_hash, table_size * sizeof(unsigned long));
  return tab2;
}

#endif
