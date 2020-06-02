#ifndef ERROR_C
#define ERROR_C

#include <stdnoreturn.h>
#include <stdio.h>
#include <stdlib.h>

noreturn static void type_error(char* expected, char* recieved);
noreturn static void throw_error(char* message);

noreturn static void type_error(char* expected, char* recieved)
{
  static char error_message[80];
  sprintf(error_message, "Type Error. Expected type '%s' but recieved type '%s'", expected, recieved);
  throw_error(error_message);
}

noreturn static void throw_error(char* message)
{
  printf("%s\n", message);
  exit(-1);
}


#endif
