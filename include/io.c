#ifndef IO_C
#define IO_C

#include "cognate.h"
#include "type.c"
#include <stdio.h>

static void print_object (const cognate_object object, const _Bool quotes);

static void print_object (const cognate_object object, const _Bool quotes)
{
  switch (object.type)
  {
    // Double precision float has 15sf precision.
    // Switch to scientific notation after 15 digits to eliminate rounding errors.
    case number: printf("%.15g", object.number);  break;
    case string: 
    {
      // Quotes is whether or not to print strings with quotes.
      if (quotes)
      {
        printf("%s", object.string);
        break;
      }
      printf("'");
      for (const char* c = object.string; *c != '\0'; ++c)
      {
        
        switch (*c)
        {
          case '\\': printf("\\");  break;
          case '\a': printf("\\a"); break;
          case '\b': printf("\\b"); break;
          case '\f': printf("\\f"); break;
          case '\n': printf("\\n"); break;
          case '\r': printf("\\r"); break;
          case '\t': printf("\\t"); break;
          case '\v': printf("\\v"); break;
          case '\'': printf("\\'"); break;
          default: printf("%c", *c);
        }
      }
      printf("'");
      break;
    }
    case list: 
    { 
      const cognate_list lst = *object.list;
      printf("[");
      for (cognate_object *i = lst . start; i != lst . top; ++i)
      {
        // Probably don't need this check.
        if (!i) throw_error("Null pointer in print_object(). This is definitely a compiler bug!");
        // Strings within lists ALWAYS printed with quotes.
        print_object(*i, 0);
        if (i + 1 != lst . top) printf(", ");
      }
      printf("]");
      break;
    }
    case boolean: printf("%s", object.boolean ? "True" : "False"); break;
    case block: throw_error("Cannot print a block!");
    default:;
  }
}

#endif
