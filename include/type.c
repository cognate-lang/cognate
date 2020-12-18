#ifndef TYPE_C
#define TYPE_C

#include "cognate.h"
#include "error.c"
#include <stddef.h>


typedef enum 
{
  // NOTHING is currently only used for unused hashtable buckets.
  NOTHING=0, // Must be zero because of calloc()
  block, 
  boolean, 
  string, 
  number, 
  list,
  table,
} cognate_type;

typedef void(^cognate_block)();

struct cognate_list
{
  struct cognate_object* start;
  struct cognate_object* top;
};


struct cognate_table
{
  struct cognate_list items;
  long unsigned int* confirmation_hash;
};

typedef struct cognate_table cognate_table;


// __attribute__((packed)) could save memory here.
struct cognate_object
{
  union
  {
    const char* string;            // 64bit string pointer
    _Bool boolean;                 //  1bit bool
    cognate_block block;           // 64bit block pointer
    double number;                 // 64bit float
    const struct cognate_list  *list;  // 64bit list pointer
    const struct cognate_table *table; // 64bit table pointer
  };
  cognate_type type : 4;
};

typedef struct cognate_object cognate_object;
typedef struct cognate_list   cognate_list;

static cognate_object check_type(cognate_type, cognate_object);
static const char* lookup_type(cognate_type);
static _Bool compare_objects(cognate_object, cognate_object);
static _Bool compare_lists(cognate_list, cognate_list);

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
  throw_error("Attempted to lookup invalid type <%i>", type);
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
    case string:  return strcmp(ob1.string, ob2.string) == 0; break;
    case list:    return compare_lists(*ob1.list, *ob2.list); break;
    case table:   return compare_lists(ob1.table->items, ob2.table->items); break;
    case block:   throw_error("Cannot compare blocks!");
    default:      return 0;
    // Records are a lie.
  }
}


#endif
