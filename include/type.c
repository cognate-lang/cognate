#ifndef TYPE_C
#define TYPE_C

#include <stdio.h>
#include "error.c"
#include "Block.h"
#include <stddef.h>
#include <ctype.h>


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

static cognate_object check_type(cognate_type expected_type, cognate_object object)
{
#ifdef unsafe 
  return object;
#else
  if (likely(object.type == expected_type)) 
  {
    return object;
  }
  type_error(lookup_type(expected_type), lookup_type(object.type));
#endif
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

unsigned int hash(const char *str, unsigned long table_size)
{
  // This simple hashing algorithm works quite well,
  unsigned int hash_val = 0;
  for (int i = strlen(str) - 1; i >= 0; i--)
  {
    hash_val = hash_val * 31 + tolower(str[i]);
  }
  return hash_val % table_size;
}

unsigned int confirm_hash(const char *str)
{
  // Confirmation hash should prevent collisions.
  // There is surely a better way of doing this.
  unsigned int hash_val = 0;
  for (int i = strlen(str) - 1; i >= 0; i--)
  {
    hash_val = hash_val * 47 + tolower(str[i]);
  }
  return hash_val;
}


static void table_add(char *key, cognate_object value, cognate_table *tab)
{
  // TODO: find out if this actually works.
  unsigned long table_size = tab->items.top - tab->items.start;
  // WARNING: resizing the table will mess up hashes!!!
  unsigned long key_hash  = hash(key, table_size);
  unsigned long key_hash2 = confirm_hash(key);
  for (;;key_hash++)
  {
    if (key_hash == table_size) key_hash = 0; // WILL LOOP INDEFINITELY
    // Add to table is bucket is either empty or has the same confirmation hash (IE same key).
    if (tab->items.start[key_hash].type == NOTHING || key_hash2 == tab->confirmation_hash[key_hash])
    {
      tab->items.start[key_hash] = value;
      // Confirmation hash should prevent collisions.
      tab->confirmation_hash[key_hash] = confirm_hash(key);
      break;
    }
  }
}

cognate_object table_get(char* key, cognate_table tab)
{
  // TODO: find out if this actually works.
  unsigned long table_size = tab.items.top - tab.items.start;
  // WARNING: resizing the table will mess up hashes!!!
  unsigned long key_hash  = hash(key, table_size);
  unsigned long key_hash2 = confirm_hash(key);
  for (;;key_hash++)
  {
    if (key_hash == table_size) key_hash = 0; // WILL LOOP INDEFINITELY
    if (key_hash2 == tab.confirmation_hash[key_hash])
    {
      return tab.items.start[key_hash];
    }
  }
  throw_error("Cannot find key in table!");
}

#endif
