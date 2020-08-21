#ifndef IO_C
#define IO_C

#include <stdio.h>
#include "type.c"

static void print_object (cognate_object object, _Bool quotes);

static void print_object (cognate_object object, _Bool quotes)
{
  // This really needs refactoring. I need to work out why this fixed the previous error. 
  switch (object.type)
  {
    // Double precision float has 15sf precision.
    // Switch to scientific notation after 15 digits to eleminate rounding errors.
    case number: printf("%.15g", object.number);  break;
    // Quotes is whether or not to print with quotes.
    case string: printf(quotes ? "%s" : "\'%s\'", object.string); break;
    case list  : { 
                   cognate_list lst = *object.list;
                   printf("[");
                   for (cognate_object *i = lst . start; i != lst . top; ++i)
                   {
                     if (!i) throw_error("Null pointer exception!");
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
