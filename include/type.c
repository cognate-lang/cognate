#ifndef TYPE_C
#define TYPE_C

#include <stdio.h>
#include "error.c"
#include "Block.h"
#include <stddef.h>


typedef enum 
{
  block=0, 
  boolean, 
  string, 
  number, 
  list,
  terminator
} cognate_type;

typedef void(^cognate_block)();

static int next_type = list + 1;

struct __attribute__((packed)) cognate_list
{
  struct cognate_object *start,
                        *top;
};

// Removing packing will give slight (almost none) performance gains at cost of more memory.
struct __attribute__((packed)) cognate_object
{
  union
  {
    char* string;                  // 64bit pointer
    _Bool boolean;                 // 1bit  bool
    cognate_block block;           // 64bit pointer
    double number;                 // 64bit float
    struct cognate_list *list;     // 64bit pointer
    struct cognate_object *record; // 64bit pointer
  };
  cognate_type type : 16;
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
    default:;
  }
  static char type_number[24]; // Max 32 bit type will take up 10 digits.
  sprintf(type_number, "<%i>", type);
  return type_number;
}

static const _Bool compare_objects(cognate_object ob1, cognate_object ob2)
{
  if (ob1.type != ob2.type)
    {
      return 0; // Not equal if differing types. None of this javascript rubbish.
    }
    switch (ob1.type)
    {
      case number:  return ob1.number == ob2.number;            break;
      case boolean: return ob1.boolean == ob2.boolean;          break;
      case block:   return ob1.block == ob2.block;              break;
      case string:  return strcmp(ob1.string, ob2.string) == 0; break;
      case list:
      {
        ptrdiff_t len = ob1.list->top - ob1.list->start;
        if (len != ob2.list->top - ob2.list->start) return 0; // Not equal if differing length.
        cognate_list lst1 = *ob1.list;
        cognate_list lst2 = *ob2.list;
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
      default:
      {
        // These are probably records, compare until terminator.
        for (int i = 0 ;; ++i)
        {
          if (ob1.record[i].type == terminator || ob2.record[i].type == terminator)
          {
            if (ob1.record[i].type == terminator && ob2.record[i].type == terminator) return 1;
            return 0;
          } 
          if (!compare_objects(ob1.record[i], ob2.record[i])) return 0;
        }
      }
    }
}

#endif
