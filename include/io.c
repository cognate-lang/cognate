#ifndef IO_C
#define IO_C

#include "cognate.h"
#include "types.h"

#include <stdio.h>

static void print_object (const cognate_object object, const _Bool quotes);

static void print_object (const cognate_object object, const _Bool quotes)
{
  // TODO I want to be able to print_object to a string, so that i can have a Show function.
  switch (object.type)
  {
    // Double precision float has 15sf precision.
    // Switch to scientific notation after 15 digits to eliminate rounding errors.
    case number: printf("%.15g", object.number);  return;
    case string: 
    {
      // Quotes is whether or not to print strings with quotes.
      if (quotes)
      {
        fputs(object.string, stdout);
        return;
      }
      putchar('\'');
      for (const char* c = object.string; *c != '\0'; ++c)
      {
        
        switch (*c)
        {
          case '\\': putchar('\\');  break;
          case '\a': fputs("\\a", stdout); break;
          case '\b': fputs("\\b", stdout); break;
          case '\f': fputs("\\f", stdout); break;
          case '\n': fputs("\\n", stdout); break;
          case '\r': fputs("\\r", stdout); break;
          case '\t': fputs("\\t", stdout); break;
          case '\v': fputs("\\v", stdout); break;
          case '\'': fputs("\\'", stdout); break;
          default: putchar(*c);
        }
      }
      putchar('\'');
      return;
    }
    case list: 
    { 
      const cognate_list lst = *object.list;
      putchar('[');
      for (cognate_object *i = lst . start; i != lst . top; ++i)
      {
        // Probably don't need this check.
        if (!i) throw_error("Null pointer in print_object(). This is definitely a compiler bug!");
        // Strings within lists ALWAYS printed with quotes.
        print_object(*i, 0);
        if (i + 1 != lst . top) fputs(", ", stdout);
      }
      putchar(']');
      return;
    }
    case boolean: object.boolean ? fputs("True", stdout) : fputs("False", stdout); return;
    case block: throw_error("Cannot print a block!"); return;
    default:;
  }
}

#endif
