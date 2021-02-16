#ifndef ERROR_C
#define ERROR_C

#include "cognate.h"

_Noreturn __attribute__((format(printf, 1, 2))) static void throw_error(const char* const, ...);
static void handle_signal(int);

#include "stack.c"
#include "io.c"
#include "type.c"

#include <stdio.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <execinfo.h>
#include <signal.h>
#include <string.h>
#include <errno.h>

static const char* function_name = NULL;
static const char* word_name = NULL;

static void set_word_name(char* name) { word_name=name; } // Need this to avoid unsequenced evaluation error.

_Noreturn __attribute__((format(printf, 1, 2))) static void throw_error(const char* const fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  struct winsize term;
  // If we cannot determine the terminal size (redirected to file or something), assume width is 80.
  const char tmp_errno = errno;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &term) == -1) term.ws_col = 80;
  fputs("\n", stderr);
  // Print the title bar.
  for (unsigned char i = 0; i < (term.ws_col - 18) / 2; ++i) fputs("\342\224\200", stderr);
  fputs("\342\224\244 \033[0;1mCognate Error!\033[0m \342\224\234", stderr);
  for (unsigned char i = 0; i < (term.ws_col - 17) / 2; ++i) fputs("\342\224\200", stderr);
  // Print generic error header.
  fputs("\nCognate has encountered an unrecoverable error.\n"
         "Details are below...\n", stderr);
  // Print the function name, if inside a function.
  if (function_name || word_name) fputs("\n", stderr);
  if (function_name) fprintf(stderr, "In function '\033[0;1m%c%s\033[0m'\n", toupper(*function_name), function_name+1);
  if (word_name != function_name && word_name) fprintf(stderr, "While evaluating '\033[0;1m%c%s\033[0m'\n", toupper(*word_name), word_name+1);
  // Actually print the error message now.
  fprintf(stderr, "\n\033[31;1m");
  vfprintf(stderr, fmt, args);
  va_end(args);
  if (tmp_errno) fprintf(stderr, "\n\033[0m\033[37;2m%s", strerror(tmp_errno));
  fputs("\033[0m\n", stderr);
  // Print a backtrace.
  /*
  void *trace[5];
  size_t size = backtrace(trace, 5);
  fputs("\033[37;2mHere is a backtrace:\n", stderr);
  backtrace_symbols_fd(trace, size, STDERR_FILENO);
  fputs("\033[0m", stderr);
  */
  // Print the top 5 stack items.
  if (stack.top != stack.start)
  {
    fputs("\n\033[37;2mHere is the top of the stack:\n", stderr);
    for (unsigned char i = 0; i < 5 && stack.top != stack.start; ++i)
    { // FIXME: Inlining of stack operations may cause inaccuracies here.
      const cognate_object obj = pop();
      fprintf(stderr, "[%s]: ", lookup_type(obj.type));
      print_object(obj, stderr, 1); // FIXME: large objects will print in their entirety here.
      fputc('\n', stderr);
    }
    if (stack.top != stack.start) printf("and %li more...\n", stack.top - stack.start);
  }
  // Print the bottom row thing.
  for (unsigned char i = 0; i < term.ws_col; ++i) fputs("\342\224\200", stderr);
  // Exit, with error.
  exit(-1);
}

static void handle_signal(int sig)
{
  throw_error("Recieved signal %i (%s), exiting.", sig, strsignal(sig));
}

#endif
