#ifndef ERROR_C
#define ERROR_C

#include <stdnoreturn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

noreturn static void type_error(char* expected, char* recieved);
noreturn static void throw_error(char* message);

noreturn static void type_error(char* expected, char* recieved)
{
  static char error_message[80];
  sprintf(error_message, "Type Error!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1. Expected type '%s' but recieved type '%s'", expected, recieved);
  throw_error(error_message);
}

noreturn static void throw_error(char* message)
{
  puts("/------------------------------- Cognate Error! --------------------------------\\");
  puts("| Cognate has encountered an unrecoverable error and cannot continue execution. |");
  puts("| Useful information, such as the expected cause of error, is printed below.    |");
  while (strlen(message) > 77)
  {
    printf("| %.*s |\n", 77, message);
    message += 77;
  }
  printf("| %s |\n", message); // TODO: pad right '|'.
  exit(-1);
}


#endif
