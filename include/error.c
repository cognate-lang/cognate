#ifndef ERROR_C
#define ERROR_C

#include "cognate.h"
#include <stdio.h>
#include <ctype.h>
#include <gc/gc.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdlib.h>

const char* function_name = NULL;

__attribute__((noreturn)) static void throw_error(const char* const, ...);

__attribute__((noreturn)) static void throw_error(const char* const fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  // Here we use vsnprintf to calculate out buffer size before actually filling it with vsprintf.
  char* const message = (char*) cognate_malloc (vsnprintf(NULL, 0, fmt, args) + 1); 
  vsprintf(message, fmt, args);
  va_end(args);
  struct winsize term;
  // If we cannot determine the terminal size (redirected to file or something), assume width is 80.
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &term) == -1) term.ws_col = 80;
  long i;
  // Print the title bar.
  for (i = 0; i < (term.ws_col - 18) / 2; ++i) fputs("\342\224\200", stdout);
  fputs("\342\224\244 \033[0;1mCognate Error!\033[0m \342\224\234", stdout);
  for (i = 0; i < (term.ws_col - 17) / 2; ++i) fputs("\342\224\200", stdout);
  // Print generic error header.
  puts("\nCognate has encountered an unrecoverable error.\n"
         "Details are below:\n");
  // Print the function name, if inside a function.
  if (function_name != NULL) printf("\033[0;1mIn function '%c%s'...\033[0m\n\n", toupper(*function_name), function_name+1);
  // Actually print the error message now.
  printf("\033[31;1m%s\033[0m\n", message);
  // Print the bottom row thing.
  for (i = 0; i < term.ws_col; ++i) fputs("\342\224\200", stdout);
  // Exit, with error.
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
