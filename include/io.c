#ifndef IO_C
#define IO_C

#include "cognate.h"
#include "types.h"

static void print_object (const cognate_object object, const _Bool quotes);

#include "error.c"

#include <stdio.h>

static void print_object (const cognate_object object, const _Bool quotes)
{
  // TODO I want to be able to print_object to a string, so that i can have a Show function.
  switch (object.type)
  {
    // Double precision float has 15sf precision.
    // Switch to scientific notation after 10 digits to eliminate rounding errors.
    case number: printf("%.14g", object.number);  return;
    case string:
    {
      // Quotes is whether or not to print strings with quotes.
      if (!quotes)
      {
        fputs(object.string, stdout);
        return;
      }
      putchar('\'');
      for (const char* c = object.string; *c != '\0'; ++c)
      {
        switch (*c)
        {
          case '\a': fputs("\\a", stdout); break;
          case '\b': fputs("\\b", stdout); break;
          case '\t': fputs("\\t", stdout); break;
          case '\n': fputs("\\n", stdout); break;
          case '\v': fputs("\\v", stdout); break;
          case '\f': fputs("\\f", stdout); break;
          case '\r': fputs("\\r", stdout); break;
          case '\'': fputs("\\'", stdout); break;
          case '\\': fputs("\\\\",stdout); break;
          default: putchar(*c);
        }
      }
      putchar('\'');
      return;
    }
    case list:
    {
      putchar('(');
      FOR_LIST(i, object.list)
      {
        print_object(i->object, 1);
        if likely(i->next)
        {
          putchar(',');
          putchar(' ');
        }
      }
      putchar(')');
      return;
    }
    case boolean: fputs(object.boolean ? "True" : "False", stdout); return;
    case block: printf("<Block %p>", (void*)object.block); return;
    case table: printf("<Table %p>", (void*)object.table); return;
    default: throw_error("Cannot print object of unknown type %i. This may be a compiler bug!", object.type);
  }
}

#endif
