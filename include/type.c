#ifndef TYPE_C
#define TYPE_C

#include <stdio.h>
#include "error.c"
#include "Block.h"


typedef enum {boolean=0, string=1, block=2, number=3, list=4} cognate_type;

typedef void(^cognate_block)();

static int next_type = list + 1;

struct __attribute__((packed)) cognate_list
{
  struct cognate_object *start,
                        //*end, // For storing amount of free allocated space. Not currently needed.
                        *top;
};

// Removing packing will give slight performance gains at cost of more memory.
struct __attribute__((packed)) cognate_object
{
  union
  {
    char* string;
    _Bool boolean;
    cognate_block block;
    double number;
    struct cognate_list *list;
    struct cognate_object *record;
  };
  cognate_type type : 32;
};

typedef struct cognate_object cognate_object;
typedef struct cognate_list   cognate_list;

static cognate_object check_type(cognate_type, cognate_object);
static const char* lookup_type(cognate_type);

static cognate_object check_type(cognate_type expected_type, cognate_object object)
{
  if (object.type == expected_type) 
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
  }
  static char type_number[80];
  sprintf(type_number, "Unknown Type %i", type);
  return type_number;
}

static const _Bool compare_objects(cognate_object ob1, cognate_object ob2)
{
  if (ob1.type != ob2.type)
    {
      return 0;
    }
    switch (ob1.type)
    {
      case number:  return ob1.number == ob2.number;            break;
      case boolean: return ob1.boolean == ob2.boolean;          break;
      case block:   return ob1.block == ob2.block;              break;
      case string:  return strcmp(ob1.string, ob2.string) == 0; break;
      case list:
        for (cognate_object *i = ob1.list->start, *j = ob2.list->start; i < ob1.list->top || j < ob2.list->top;)
        {
          if (!compare_objects(*i, *j))
          {
            return 0;
          }
          ++i; ++j;
        }
        return 1;
    }
}

#endif