#include "cognate.h"

#include <Block.h>
#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <time.h>
#include <unistd.h>
#ifndef NO_GC
#include <gc/gc.h>
#endif
#if 0 //__has_include(<Block_private.h>)
// TODO
#define BLOCK_GC
#endif

#ifdef BLOCK_GC
#include <Block_private.h>
void* blk_alloc(const unsigned long size, __attribute__((unused)) const _Bool _, __attribute__((unused)) const _Bool __) { return GC_MALLOC(size); }
void blk_setHasRefcount(__attribute__((unused)) const void* _, __attribute__((unused)) const _Bool __) {}
void blk_gc_assign_strong(void* src, void** dst) { *dst = src; }
void blk_gc_assign_weak(const void* src, void* dst) { *(void**)dst = (void*)src; }
void blk_gc_memmove(void* dst, void* src, unsigned long size) { memmove(dst, src, size); }
#endif

static const char *lookup_type(cognate_type);
static _Bool compare_lists(LIST, LIST);
static _Bool compare_tables(TABLE, TABLE);
static void handle_error_signal(int);
static void bind_error_signals();

cognate_stack stack;
LIST cmdline_parameters = NULL;
const char *current_function_name = NULL;
const char *current_word_name     = NULL;

static const char *function_stack_start;
static const char *function_stack_top;

void init(int argc, char** argv)
{
  // Get function stack limit
  char a;
  function_stack_start = &a;
  struct rlimit stack_limit;
  if unlikely(getrlimit(RLIMIT_STACK, &stack_limit) == -1)
  {
    throw_error("Cannot get return stack limit!");
  }
  function_stack_top = function_stack_start - stack_limit.rlim_cur;
  // Set locale for strings.
  if unlikely(setlocale(LC_ALL, "") == NULL)
  {
    throw_error("Cannot set locale!");
  }
  // Init GC
#ifndef NO_GC
  GC_INIT();
#ifdef BLOCK_GC
  _Block_use_GC(blk_alloc, blk_setHasRefcount, blk_gc_assign_strong, blk_gc_assign_weak, blk_gc_memmove);
#else
  #pragma message "Cannot find header <Block_private.h>. Blocks cannot use garbage collection and may leak memory!"
#endif
#else
  #pragma message "Compiling without the garbage collector will cause memory leaks!"
#endif
  // Seed the random number generator properly.
  struct timespec ts;
  if unlikely(timespec_get(&ts, TIME_UTC) == 0)
  {
    throw_error("Cannot get system time!");
  }
  srand(ts.tv_nsec ^ ts.tv_sec); // TODO make random more random.
  // Load parameters
  while (argc --> 1)
  {
    cognate_list* const tmp = GC_NEW (cognate_list);
    tmp->object = OBJ(string, argv[argc]);
    tmp->next = cmdline_parameters;
    cmdline_parameters = tmp;
  }
  // Bind error signals.
  char signals[] = {SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGFPE, SIGSEGV, SIGPIPE, SIGTERM, SIGCHLD};
  for (size_t i = 0; i < sizeof(signals)/sizeof(signals[0]); ++i) signal(signals[i], handle_error_signal);
  // Initialize the stack.
  init_stack();
}

void cleanup()
{
  if unlikely(stack.top != stack.start)
  {
    current_word_name = NULL;
    throw_error("Program exiting with non-empty stack of length %ti", stack.top - stack.start);
  }
}

cognate_object copy_if_block(cognate_object obj)
{
  return unlikely(obj.type == block) ? OBJ(heap_block, Block_copy(obj.block)) : obj;
}

void copy_stack_blocks()
{
  for (cognate_object* obj = stack.top - 1; stack.uncopied_blocks; --obj)
  {
    if unlikely(obj->type == block)
    {
      *obj = OBJ(heap_block, Block_copy(obj->block));
      --stack.uncopied_blocks;
    }
  }
}

void check_function_stack_size()
{
  char sp;
  if unlikely(&sp - function_stack_top < STACK_MARGIN_KB * 1024)
    throw_error("Call stack overflow - too much recursion! (call stack is %tikB out of maximum %tikB)", (function_stack_start - &sp) >> 10, (function_stack_start - function_stack_top) >> 10);
}

void set_current_word_name(const char* const name) { current_word_name=name; } // Need this to avoid unsequenced evaluation error.

_Noreturn __attribute__((format(printf, 1, 2))) void throw_error(const char* const fmt, ...)
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
  if (current_word_name || current_function_name) fputc('\n', stderr);
  if (current_function_name) fprintf(stderr, "In function '\033[0;1m%c%s\033[0m'\n", toupper(*current_function_name), current_function_name+1);
  if ((current_word_name != current_function_name) && current_word_name)
    fprintf(stderr, "During (or immediately after) evaluation of '\033[0;1m%c%s\033[0m'\n", toupper(*current_word_name), current_word_name+1);
  // Actually print the error message now.
  fprintf(stderr, "\n\033[31;1m");
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fputc('\n', stderr);
  if (tmp_errno) fprintf(stderr, "\033[0;2m%s\n", strerror(tmp_errno));
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

void handle_error_signal(int sig)
{
  throw_error("Recieved signal %i (%s), exiting.", sig, strsignal(sig));
}

void print_object (const cognate_object object, FILE* out, const _Bool quotes)
{
  // TODO I want to be able to print_object to a string, so that i can have a Show function.
  switch (object.type)
  {
    // Double precision float has 15sf precision.
    // Switch to scientific notation after 10 digits to eliminate rounding errors.
    case number: fprintf(out, "%.14g", object.number);  return;
    case string:
    {
      // Quotes is whether or not to print strings with quotes.
      if (!quotes)
      {
        fputs(object.string, out);
        return;
      }
      fputc('\'', out);
      char c;
      for (const char* ptr = object.string; (c = *ptr) != '\0'; ++ptr)
      {
        if (c >= '\a' && c <= '\r')
        {
          fputc('\\', out);
          fputc("abtnvfr"[c-'\a'], out);
        }
        else if (c == '\\') fputs("\\\\", out);
        else if (c == '\'') fputs("\\\'", out);
        else fputc(c, out);
      }
      fputc('\'', out);
      return;
    }
    case list:
    {
      fputc('(', out);
      for (LIST i = object.list; i ; i = i->next)
      {
        print_object(i->object, out, 1);
        if likely(i->next)
        {
          fputs(", ", out);
        }
      }
      fputc(')', out);
      return;
    }
    case boolean: fputs(object.boolean ? "True" : "False", out); return;
    case block: fprintf(out, "<Block %p>", (void*)object.block); return;
    case block | heap_block: fprintf(out, "<Block %p>", (void*)object.block); return;
    case table: fprintf(out, "<Table %p>", (void*)object.table); return;
    default: throw_error("Cannot print object of unknown type %i. This may be a compiler bug!", object.type);
  }
}

void init_stack()
{
  stack.uncopied_blocks = 0;
  stack.size = INITIAL_LIST_SIZE;
  stack.top = stack.start =
    (cognate_object*) GC_MALLOC (INITIAL_LIST_SIZE * sizeof(cognate_object));
}

void push(cognate_object object)
{
  if unlikely(stack.start + stack.size == stack.top)
    expand_stack();
  stack.uncopied_blocks += (object.type == block);
  *stack.top++ = object;
}

cognate_object pop()
{
  if unlikely(stack.top == stack.start)
    throw_error("Stack underflow!");
  const cognate_object object = *--stack.top;
  stack.uncopied_blocks -= (object.type == block);
  return object;
}

cognate_object peek()
{
  if unlikely(stack.top == stack.start)
    throw_error("Stack underflow!");
  return *(stack.top - 1);
}

void expand_stack()
{
  // Assumes that stack is currently of length stack.size.
  stack.start = (cognate_object*) GC_REALLOC (stack.start, stack.size * LIST_GROWTH_FACTOR * sizeof(cognate_object));
  stack.top = stack.start + stack.size;
  stack.size *= LIST_GROWTH_FACTOR;
}

/*
unsigned long hash(const char *str)
{
  // http://www.cse.yorku.ca/~oz/hash.html
  unsigned long hash = 0;
  int c;
  while ((c = *str++))
    hash = c + (hash << 6) + (hash << 16) - hash;
  return hash;
}
*/

cognate_object check_type(cognate_type expected_type, cognate_object object)
{
  if likely(object.type & expected_type)
    return object;
  // TODO: Print the object itself here.
  throw_error("Type Error! Expected type '%s' but recieved type '%s'", lookup_type(expected_type), lookup_type(object.type));
}

const char* lookup_type(cognate_type type)
{
  char str[] = "\0oolean/String/Number/List/Table/Block";
  if (!type) return "NOTHING";
  if (type & boolean) strcat(str, "/Boolean");
  if (type & string)  strcat(str, "/String");
  if (type & number)  strcat(str, "/Number");
  if (type & list)    strcat(str, "/List");
  if (type & table)   strcat(str, "/Table");
  if (type & (block | heap_block)) strcat(str, "/Block");
  return GC_STRDUP(str + 1);
}

_Bool compare_lists(LIST lst1, LIST lst2)
{
  if (!lst1) return !lst2;
  if (!lst2) return 0;
  while (compare_objects(lst1->object, lst2->object))
  {
    if (!lst1->next) return !lst2->next;
    if (!lst2->next) return 0;
    lst1 = lst1 -> next;
    lst2 = lst2 -> next;
  }
  return 0;
}

_Bool compare_tables(TABLE tab1, TABLE tab2)
{
  (void) tab1;
  (void) tab2;
  return 1; // TODO
}

_Bool compare_objects(cognate_object ob1, cognate_object ob2)
{
  if (!(ob1.type & ob2.type))
  {
    return 0; // Not equal if differing types. None of this javascript rubbish.
  }
  switch (ob1.type)
  {
    case number     : return ob1.number  == ob2.number;
    case boolean    : return ob1.boolean == ob2.boolean;
    case string     : return strcoll(ob1.string, ob2.string) == 0;
    case list       : return compare_lists(ob1.list, ob2.list);
    case table      : return 0; // compare_tables(*ob1.table, *ob2.table);
    case NOTHING    : throw_error("Cognate should not be in this state - compiler bug!");
    case block      : throw_error("Cannot compare blocks!");
    case heap_block : throw_error("Cannot compare blocks!");
  }
}


