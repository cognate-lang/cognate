#ifndef TYPE_H
#define TYPE_H

#include <Block.h>
#include <stddef.h>

enum cognate_type
{
  // NOTHING is currently only used for unused hashtable buckets.
  NOTHING = 0, // Must be zero because of calloc()
  boolean = (1 << 0),
  string  = (1 << 1),
  number  = (1 << 2),
  list    = (1 << 3),
  table   = (1 << 4),
  block   = (1 << 5),
  heap_block = (1 << 5) | (1 << 6)
};

typedef struct cognate_table cognate_table;
typedef void(^cognate_block)();
typedef enum cognate_type cognate_type;
typedef struct cognate_object cognate_object;
typedef struct cognate_list   cognate_list;

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

// __attribute__((packed)) could save memory here.
struct cognate_object
{
  union
  {
    const char* string;                // 64bit string pointer
    _Bool boolean;                     //  1bit bool
    cognate_block block;               // 64bit block pointer
    cognate_block stack_block;               // 64bit block pointer
    double number;                     // 64bit float
    const struct cognate_list  *list;  // 64bit list pointer
    const struct cognate_table *table; // 64bit table pointer
  };
  cognate_type type : 8;
};

#endif
