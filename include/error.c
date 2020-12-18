#ifndef ERROR_C
#define ERROR_C

#include "cognate.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define noreturn __attribute__ ((noreturn))

char* function_name = NULL;

noreturn static void throw_error(const char*, ...);

noreturn static void type_error(const char* const expected, const char* const recieved);
noreturn static void type_error(const char* const expected, const char* const recieved)
{
  // In future, type errors should instead be 'expectation errors'.
  // These print an expected predicate, and the value that didn't satisfy it.
  // This allows the language to move away from types in future.
  throw_error("Type Error! Expected type '%s' but recieved type '%s'", expected, recieved);
}

noreturn static void throw_error(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  char* message = malloc(sizeof(char) * (MAX_ERRORMSG_LEN + 1));
  vsprintf(message, fmt, args);
  // Uses utf8 box-drawing characters and ansi colour codes to print a pretty error message.
  int i;
  printf("\n\342\224\214");
  for (i = 0; i < 30; ++i) printf("\342\224\200");
  printf("\342\224\244 \033[0;1mCognate Error!\033[0m \342\224\234");
  for (i = 0; i < 30; ++i) printf("\342\224\200");
  puts("\342\224\220");
  puts("\342\224\202 Cognate has reached an unrecoverable state and cannot continue execution.    \342\224\202");
  puts("\342\224\202 The expected cause of error is printed below.                                \342\224\202");
  puts("\342\224\202                                                                              \342\224\202");
  if (function_name != NULL)
  {
    // Print first character seperately so that we can capitalise it without modifying the read-only string.
    if (strlen(function_name) > 60)
    {
      function_name = "[FUNCTION NAME TOO LONG]";
    }
    printf("\342\224\202 \033[0;1mIn function '%c%s'...\033[0m", *function_name-32, function_name+1);
    for (i = 60-strlen(function_name); i>0; --i) printf(" ");
    puts("\342\224\202");
  }
  int len = strlen(message);
  while (len > 76)
  {
    printf("\342\224\202 \033[31;1m%.*s\033[0m \342\224\202\n", 76, message);
    message += 76;
    len     -= 76;
  }
  printf("\342\224\202 \033[31;1m%s\033[0m", message);
  for (i = 77 - strlen(message); i > 0; --i) putchar(' ');
  printf("\342\224\202\n\342\224\224");
  for (i = 0; i < 78; ++i) printf("\342\224\200");
  puts("\342\224\230");
  exit(-1);
}

static void debug_printf(__attribute__((unused)) const char* fmt, ...)
{
#if debug
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "[DEBUG] %s:%d -> ", __FILE__, __LINE__);
  vfprintf(stderr, fmt, args);
  puts("");
#endif
}

#endif
