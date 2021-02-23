#ifndef IO_C
#define IO_C

#include "cognate.h"
#include "types.h"

#include <stdio.h>

static void print_object (const cognate_object object, FILE* out, const _Bool quotes);

#include "error.c"

static void print_object (const cognate_object object, FILE* out, const _Bool quotes)
{
  // TODO I want to be able to print_object to a string, so that i can have a Show function.
  switch (object.type)
  {
    // Double precision float has 15sf precision.
    // Switch to scientific notation after 10 digits to eliminate rounding errors.
    case number: fprintf(out, "%.14g", object.number);  return;
    case string:
    {
      // Quotes is whether or not to print strings with quotes.
      if (!quotes)
      {
        fputs(object.string, out);
        return;
      }
      fputc('\'', out);
      for (const char* c = object.string; *c != '\0'; ++c)
      {
        switch (*c)
        {
          case '\a': fputs("\\a", out); break;
          case '\b': fputs("\\b", out); break;
          case '\t': fputs("\\t", out); break;
          case '\n': fputs("\\n", out); break;
          case '\v': fputs("\\v", out); break;
          case '\f': fputs("\\f", out); break;
          case '\r': fputs("\\r", out); break;
          case '\'': fputs("\\'", out); break;
          case '\\': fputs("\\\\",out); break;
          default: fprintf(out, "%c", *c);
        }
      }
      fputc('\'', out);
      return;
    }
    case list:
    {
      fputc('(', out);
      for (cognate_list i = object.list; i ; i = i->next)
      {
        print_object(i->object, out, 1);
        if likely(i->next)
        {
          fputs(", ", out);
        }
      }
      fputc(')', out);
      return;
    }
    case boolean: fputs(object.boolean ? "True" : "False", out); return;
    case block: fprintf(out, "<Block %p>", (void*)object.block); return;
    case block | heap_block: fprintf(out, "<Block %p>", (void*)object.block); return;
    case table: fprintf(out, "<Table %p>", (void*)object.table); return;
    default: throw_error("Cannot print object of unknown type %i. This may be a compiler bug!", object.type);
  }
}

#endif
