#ifndef TYPE_C
#define TYPE_C

#include "cognate.h"
#include "types.h"

static cognate_object check_type(cognate_type, cognate_object);
static const char* lookup_type(cognate_type);
static _Bool compare_objects(cognate_object, cognate_object);
static _Bool compare_lists(cognate_list, cognate_list);
static _Bool compare_tables(cognate_table, cognate_table);

#include "error.c"
#include <string.h>
#include <stdlib.h>

static cognate_object check_type(cognate_type expected_type, cognate_object object)
{
  if likely(object.type & expected_type)
    return object;
  // TODO: Print the object itself here.
  throw_error("Type Error! Expected type '%s' but recieved type '%s'", lookup_type(expected_type), lookup_type(object.type));
}

static const char* lookup_type(cognate_type type)
{
  char str[54] = {0};
  if (!type) return "NOTHING";
  if (type & boolean) strcat(str, "/Boolean");
  if (type & string)  strcat(str, "/String");
  if (type & number)  strcat(str, "/Number");
  if (type & list)    strcat(str, "/List");
  if (type & table)   strcat(str, "/Table");
  if (type & (block | heap_block)) strcat(str, "/Block");
  return GC_STRDUP(str + 1);
}

static _Bool compare_lists(cognate_list lst1, cognate_list lst2)
{
  if (!lst1) return !lst2;
  if (!lst2) return 0;
  while (compare_objects(lst1->object, lst2->object))
  {
    if (!lst1->next) return !lst2->next;
    if (!lst2->next) return 0;
    lst1 = lst1 -> next;
    lst2 = lst2 -> next;
  }
  return 0;
}

static _Bool compare_tables(const cognate_table tab1, const cognate_table tab2)
{
  (void) tab1;
  (void) tab2;
  return 1; // TODO
}

static _Bool compare_objects(cognate_object ob1, cognate_object ob2)
{
  if (!(ob1.type & ob2.type))
  {
    return 0; // Not equal if differing types. None of this javascript rubbish.
  }
  switch (ob1.type)
  {
    case number     : return ob1.number  == ob2.number;
    case boolean    : return ob1.boolean == ob2.boolean;
    case string     : return strcoll(ob1.string, ob2.string) == 0;
    case list       : return compare_lists(ob1.list, ob2.list);
    case table      : return 0; // compare_tables(*ob1.table, *ob2.table);
    case NOTHING    : throw_error("Cognate should not be in this state - compiler bug!");
    case block      : throw_error("Cannot compare blocks!");
    case heap_block : throw_error("Cannot compare blocks!");
  }
}

static size_t mbstrlen(const char* str)
{
  // Get the number of characters in a multibyte string.
  // Normal strlen() gets number of bytes for some reason.
  size_t len = 0;
  for (; *str ; str += mblen(str, MB_CUR_MAX), ++len);
  return len;
}

#endif
