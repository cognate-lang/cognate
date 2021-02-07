#ifndef TYPE_H
#define TYPE_H

#include <Block.h>
#include <stddef.h>
#include <stdbool.h>

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

typedef void(^cognate_block)();
typedef _Bool cognate_boolean;
typedef double cognate_number;
typedef const char* cognate_string;
typedef const struct cognate_list_node* cognate_list;
typedef struct cognate_table cognate_table;

typedef enum cognate_type cognate_type;
typedef struct cognate_object cognate_object;
typedef struct cognate_stack  cognate_stack;
typedef struct cognate_list_node cognate_list_node;

// __attribute__((packed)) could save memory here.
struct cognate_object
{
  union
  {
    cognate_boolean boolean;   //  1bit bool
    cognate_block block;       // 64bit block pointer
    cognate_block stack_block; // 64bit block pointer
    cognate_number number;     // 64bit float
    cognate_string string;     // 64bit string pointer
    cognate_list list;    // 64bit list pointer
    const cognate_table* table;  // 64bit table-id
  };
  cognate_type type : 8;
};


struct cognate_list_node
{
  cognate_list next;
  cognate_object object;
};

struct table_bucket
{
  cognate_object object;
  const char* key; // TODO: Decrease space usage of these - store a second hash or something.
  const long id;
};

struct cognate_table
{
  _Bool todo;
};

struct cognate_stack
{
  cognate_object* start; // Pointer to start.
  cognate_object* top; // Pointer to top.
  ptrdiff_t       size; // Allocated size of the stack.
  size_t          uncopied_blocks; // Number of uncopied cognate_blocks on the stack.
};

#endif
