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
    // Quotes is whether or not to print strings with quotes.
    // TODO should show newlines as \n when in quotes mode.
    case string: printf(quotes ? "%s" : "\'%s\'", object.string); break;
    case list  : { 
                   const cognate_list lst = *object.list;
                   printf("[");
                   for (cognate_object *i = lst . start; i != lst . top; ++i)
                   {
                     if (!i) throw_error("Null pointer in print_object(). This is definitely a compiler bug!");
                     // Strings within lists ALWAYS printed with quotes.
                     print_object(*i, 0);
                     if (i + 1 != lst . top) printf(", ");
                   }
                   printf("]");
                 }
                 break;
    case boolean: printf("%s", object.boolean ? "True" : "False"); break;
    case block  : printf("<BLOCK>");
    default : printf("<%i>", object.type);
  }
}

#endif
