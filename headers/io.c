#ifndef IO_C
#define IO_C

#include <stdio.h>
#include "type.c"
#include "debug.c"

static void print_object (cognate_object object);

static void print_object (cognate_object object)
{
  // This really needs refactoring. I need to work out why this fixed the previous error. 
  switch (object.type)
  {
    // Double precision float has 15sf precision.
    // Switch to scientific notation after 15 digits to eleminate rounding errors.
    case number: printf("%.15g", object.number); break;

    case list  : { 
                   cognate_list lst = *object.list;
                   printf("[");
                   for (cognate_object *i = lst . start; i != lst . top; ++i)
                   {
                     if (i == NULL) throw_error("Null pointer exception!");
                     print_object(*i); // Segfault when trying to dereference i here.
                     if (i + 1 != lst . top) printf(", ");
                   }
                   printf("]");
                 }
                 break;
    default    : type_error("Number or List", lookup_type(object.type)); break;
  }
}

#endif
