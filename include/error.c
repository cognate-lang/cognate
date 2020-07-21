#ifndef ERROR_C
#define ERROR_C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define noreturn __attribute__ ((noreturn))

noreturn static void type_error(const char* expected, const char* recieved);
noreturn static void throw_error(const char* message);

noreturn static void type_error(const char* expected, const char* recieved)
{
  static char error_message[80];
  sprintf(error_message, "Type Error! Expected type '%s' but recieved type '%s'", expected, recieved);
  throw_error(error_message);
}

noreturn static void throw_error(const char* message)
{
  // Uses utf8 box-drawing characters and ansi colour codes to print a pretty error message.
  int i;
  printf("\342\224\214");
  for (i = 0; i < 30; ++i) printf("\342\224\200");
  printf("\342\224\244 \033[0;1mCognate Error!\033[0m \342\224\234");
  for (i = 0; i < 30; ++i) printf("\342\224\200");
  puts("\342\224\220");
  puts("\342\224\202 Cognate has reached an unrecoverable state and cannot continue execution.    \342\224\202");
  puts("\342\224\202 The expected cause of error is printed below.                                \342\224\202");
  puts("\342\224\202                                                                              \342\224\202");
  while (strlen(message) > 76)
  {
    printf("\342\224\202 \033[31;1m%.*s\033[0m \342\224\202\n", 76, message);
    message += 76;
  }
  printf("\342\224\202 \033[31;1m%s\033[0m", message);
  for (i = 77 - strlen(message); i > 0; --i) putchar(' ');
  printf("\342\224\202\n\342\224\224");
  for (i = 0; i < 78; ++i) printf("\342\224\200");
  puts("\342\224\230");
  exit(-1);
}


#endif
