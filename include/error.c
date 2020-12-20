#ifndef ERROR_C
#define ERROR_C

#include "cognate.h"
#include <stdio.h>
#include <ctype.h>
#include <gc.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

const char* function_name = NULL;

__attribute__((noreturn)) static void type_error(const char* const expected, const char* const recieved);
__attribute__((noreturn)) static void throw_error(const char* const, ...);

__attribute__((noreturn)) static void type_error(const char* const expected, const char* const recieved)
{
  throw_error("Type Error! Expected type '%s' but recieved type '%s'", expected, recieved);
}

__attribute__((noreturn)) static void throw_error(const char* const fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  char* const message = (char*) cognate_malloc (vsnprintf(NULL, 0, fmt, args) + 1); 
  vsprintf(message, fmt, args); // We use vsnprintf to determine out buffer size - we don't care about speed here.
  va_end(args);
  struct winsize term;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &term);
  long i;
  for (i = 0; i < (term.ws_col - 18) / 2; ++i) fputs("\342\224\200", stdout);
  fputs("\342\224\244 \033[0;1mCognate Error!\033[0m \342\224\234", stdout);
  for (i = 0; i < (term.ws_col - 17) / 2; ++i) fputs("\342\224\200", stdout);
  puts("\nCognate has encountered an unrecoverable error.\n"
         "Details are below:\n");
  if (function_name != NULL) printf("\033[0;1mIn function '%c%s'...\033[0m\n\n", toupper(*function_name), function_name+1);
  printf("\033[31;1m%s\033[0m\n", message);
  for (i = 0; i < term.ws_col; ++i) fputs("\342\224\200", stdout);
  exit(-1);
}

static void debug_printf(__attribute__((unused)) const char* fmt, ...)
{
#if debug
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "[DEBUG] %s:%d -> ", __FILE__, __LINE__);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fputc('\n', stderr);
#endif
}

#endif
