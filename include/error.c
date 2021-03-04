#ifndef ERROR_C
#define ERROR_C

#include "cognate.h"

_Noreturn __attribute__((format(printf, 1, 2))) static void throw_error(const char* const, ...);
_Noreturn static void handle_signal(int);
static void bind_signals();

#include "stack.c"
#include "io.c"
#include "type.c"

#include <stdio.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <setjmp.h>

static const char* function_name = NULL;
static const char* word_name = NULL;

static sigjmp_buf signal_jmp;

static void set_word_name(const char* const name) { word_name=name; } // Need this to avoid unsequenced evaluation error.

_Noreturn __attribute__((format(printf, 1, 2))) static void throw_error(const char* const fmt, ...)
{
  struct winsize term;
  // If we cannot determine the terminal size (redirected to file or something), assume width is 80.
  const char tmp_errno = errno;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &term) == -1) term.ws_col = 80;
  fputc('\n', stderr);
  // Print the title bar.
  for (unsigned short i = 0; i < (term.ws_col - 18) / 2; ++i) fputs("\342\224\200", stderr);
  fputs("\342\224\244 \033[0;1mCognate Error!\033[0m \342\224\234", stderr);
  for (unsigned short i = 0; i < (term.ws_col - 17) / 2; ++i) fputs("\342\224\200", stderr);
  // Print generic error header.
  fputs("\nCognate has encountered an unrecoverable error.\n"
         "Details are below...\n", stderr);
  // Print the function name, if inside a function.
  if (word_name || function_name) fputc('\n', stderr);
  if (function_name) fprintf(stderr, "In function '\033[0;1m%c%s\033[0m'\n", toupper(*function_name), function_name+1);
  if ((word_name != function_name) && word_name) fprintf(stderr, "While evaluating '\033[0;1m%c%s\033[0m'\n", toupper(*word_name), word_name+1);
  // Actually print the error message now.
  fprintf(stderr, "\n\033[31;1m");
  (void)fmt;
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, "\n\033[0;2m%s\n", tmp_errno ? strerror(tmp_errno) : "");
  // Print the top 5 stack items.
  if (stack.top != stack.start)
  {
    fputs("\n\033[0;2mHere is the top of the stack:\n", stderr);
    for (unsigned char i = 0; i < 5 && (stack.top != stack.start); ++i)
    { // FIXME: Inlining of stack operations may cause inaccuracies here.
      const cognate_object obj = pop();
      fprintf(stderr, "[%s]: ", lookup_type(obj.type));
      print_object(obj, stderr, 1); // FIXME: large objects will print in their entirety here.
      fputc('\n', stderr);
    }
    if (stack.top != stack.start) fprintf(stderr, "and %li more...\n", stack.top - stack.start);
  }
  fputs("\033[0m", stderr);
  // Print the bottom row thing.
  for (unsigned short i = 0; i < term.ws_col; ++i) fputs("\342\224\200", stderr);
  // Exit, with error.
  exit(EXIT_FAILURE);
}

_Noreturn static void handle_signal(int sig)
{
  // Can't print a fancy error message here, since we are using a resrticted stack.
  longjmp(signal_jmp, sig);
}

static void bind_signals()
{
  char sig_stack_start; // Use the old stack as the signal stack - this is probably undefined.
  // static char sig_stack_start[MINSIGSTKSZ]; // Use this line if the above line breaks.
  const stack_t signal_stack = {.ss_sp=&sig_stack_start, .ss_size=MINSIGSTKSZ};
  const struct sigaction signal_action = {.sa_handler=handle_signal, .sa_flags=SA_ONSTACK, .sa_mask={0}};
  sigaltstack(&signal_stack, NULL);
  sigaction(SIGHUP,  &signal_action, NULL);
  sigaction(SIGINT,  &signal_action, NULL);
  sigaction(SIGQUIT, &signal_action, NULL);
  sigaction(SIGILL,  &signal_action, NULL);
  sigaction(SIGABRT, &signal_action, NULL);
  sigaction(SIGBUS,  &signal_action, NULL);
  sigaction(SIGFPE,  &signal_action, NULL);
  sigaction(SIGSEGV, &signal_action, NULL);
  sigaction(SIGPIPE, &signal_action, NULL);
  sigaction(SIGTERM, &signal_action, NULL);
  sigaction(SIGCHLD, &signal_action, NULL);
  int sig = sigsetjmp(signal_jmp, 0);
  switch (sig)
  {
    case SIGSEGV: throw_error("Call stack overflow - too much recursion");
    case 0: return;
    default: throw_error("Recieved signal %i (%s), exiting.", sig, strsignal(sig));
  }
}

#endif
