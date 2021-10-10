#include "runtime.h"
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <locale.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <time.h>
#include <math.h>
#include <gc/gc.h>

static const char *lookup_type(cognate_type);
static _Bool compare_lists(LIST, LIST);
static _Bool compare_records(RECORD, RECORD);
static void handle_error_signal(int);

cognate_stack stack;
LIST cmdline_parameters = NULL;

const char* restrict word_name = NULL;
int line_num = -1;

static const char* restrict function_stack_start;
static const char* restrict function_stack_top;

void init(int argc, char** argv)
{
  // Get function stack limit
  char a;
  function_stack_start = &a;
  struct rlimit stack_limit;
  if unlikely(getrlimit(RLIMIT_STACK, &stack_limit) == -1)
  {
    throw_error("cannot get return stack limit");
  }
  function_stack_top = function_stack_start - stack_limit.rlim_cur;
  // Set locale for strings.
  if unlikely(setlocale(LC_ALL, "") == NULL)
  {
    throw_error("cannot set locale");
  }
  // Init GC
  GC_INIT();
  // Seed the random number generator properly.
  struct timespec ts;
  if unlikely(clock_gettime(CLOCK_REALTIME, &ts) == -1)
  {
    throw_error("cannot get system time");
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
  char signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGFPE, SIGSEGV, SIGPIPE, SIGTERM, SIGCHLD };
  for (size_t i = 0; i < sizeof(signals); ++i) signal(signals[i], handle_error_signal);
  // Initialize the stack.
  init_stack();
}

void cleanup()
{
  if unlikely(stack.top != stack.start || stack.cache.type)
  {
    word_name = NULL;
    throw_error_fmt("exiting with %ti object(s) on the stack", stack.top - stack.start + (stack.cache.type != 0));
  }
}

ANY copy_if_block(ANY obj)
{
  if unlikely(obj.type == block) obj.block = Block_copy(obj.block);
  return obj;
}

void check_function_stack_size()
{
  const char sp;
  if unlikely(&sp - function_stack_top < STACK_MARGIN_KB * 1024)
    throw_error("maximum recursion depth exceeded");
}

void set_word_name(const char* restrict const name) { word_name=name; } // Need this to avoid unsequenced evaluation error.
void set_line_num(int num) { line_num=num; } // Need this to avoid unsequenced evaluation error.

_Noreturn __attribute__((format(printf, 1, 2))) void throw_error_fmt(const char* restrict const fmt, ...)
{
  const _Bool debug = word_name && line_num != -1;
  int offset = 0;
  fputc('\n', stderr);
  if (debug)
  {
    int line_num_digits = 1;
    for (int tmp = line_num; tmp /= 10; ++line_num_digits);
    offset = strlen("Line: ... ") + line_num_digits + strlen(word_name);
    fprintf(stderr, "\033[0;2mLine %i: \033[0;1m... %c%s ...\n%*s\033[31;1m↳ ", line_num, toupper(*word_name), word_name + 1, offset, "");
  } else fputs("\033[31;1m", stderr);
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  fputc('\n', stderr);
  if (errno)
  {
    const char* str = strerror(errno);
    fprintf(stderr, "%*s\033[0;2m%c%s\n", offset + 2, "", tolower(*str), str+1);
  }
  fputs("\033[0m", stderr);
  exit(EXIT_FAILURE);
}

_Noreturn void throw_error(const char* restrict const msg)
{
  const _Bool debug = word_name && line_num != -1;
  int offset = 0;
  fputc('\n', stderr);
  if (debug)
  {
    int line_num_digits = 1;
    for (int tmp = line_num; tmp /= 10; ++line_num_digits);
    offset = strlen("Line: ... ") + line_num_digits + strlen(word_name);
    fprintf(stderr, "\033[0;2mLine %i: \033[0;1m... %c%s ...\n%*s\033[31;1m↳ ", line_num, toupper(*word_name), word_name + 1, offset, "");
  } else fputs("\033[31;1m", stderr);
  fputs(msg, stderr);
  fputc('\n', stderr);
  if (errno)
  {
    const char* str = strerror(errno);
    fprintf(stderr, "%*s\033[0;2m%c%s\n", offset + 2, "", tolower(*str), str+1);
  }
  fputs("\033[0m", stderr);
  exit(EXIT_FAILURE);
}



void handle_error_signal(int sig)
{
  throw_error_fmt("recieved signal %i (%s)", sig, strsignal(sig));
}

void print_object (const ANY object, FILE* out, const _Bool quotes)
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
      for (const char* restrict ptr = object.string; (c = *ptr) != '\0'; ++ptr)
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
        if likely(i->next) fputc(' ', out);
      }
      fputc(')', out);
      return;
    }
    case record:
    {
      RECORD rec = object.record;
      fputc('(', out);
      for (size_t i = 0; i < rec->len; ++i)
      {
        fprintf(out, "%s: ", rec->items[i].name);
        print_object(rec->items[i].object, out, 1);
        if likely(i + 1 < rec->len) fputs(", ", out);
      }
      fputc(')', out);
      return;
    }
    case boolean: fputs(object.boolean ? "True" : "False", out); return;
    case block: fprintf(out, "<Block %p>", (void*)object.block); return;
    case symbol: fputs(object.symbol, out); return;
    case NOTHING: __builtin_trap();
  }
}

void init_stack()
{
  stack.size = INITIAL_LIST_SIZE;
  stack.top = stack.start = GC_MALLOC (INITIAL_LIST_SIZE * sizeof(ANY));
  stack.cache.type = NOTHING;
}

void push(ANY object)
{
  if unlikely(object.type == block) object.block = Block_copy(object.block);
  if (!stack.cache.type) { stack.cache = object; return; }
  if unlikely(stack.start + stack.size == stack.top) expand_stack();
  *stack.top++ = stack.cache;
  stack.cache = object;
}

ANY pop()
{
  if (stack.cache.type) { const ANY a = stack.cache; stack.cache.type = NOTHING; return a; }
  if unlikely(stack.top == stack.start) throw_error("stack underflow");
  return *--stack.top;
}

ANY peek()
{
  if (stack.cache.type) return stack.cache;
  if unlikely(stack.top == stack.start) throw_error("stack underflow");
  return *(stack.top - 1);
}

int stack_length()
{
  return stack.top - stack.start + (stack.cache.type != NOTHING);
}

void expand_stack()
{
  // Assumes that stack is currently of length stack.size.
  stack.start = (ANY*) GC_REALLOC (stack.start, stack.size * LIST_GROWTH_FACTOR * sizeof(ANY));
  stack.top = stack.start + stack.size;
  stack.size *= LIST_GROWTH_FACTOR;
}

ANY check_type(cognate_type expected_type, ANY object)
{
  if likely(object.type & expected_type) return object;
  // TODO: Print the object itself here.
  throw_error_fmt("expected %s got %s", lookup_type(expected_type), lookup_type(object.type));
}

const char* lookup_type(cognate_type type)
{
  char str[] = "\0oolean/String/Number/List/Block/Record/Symbol";
  if (!type) return "NOTHING";
  if (type & boolean) strcat(str, "/Boolean");
  if (type & string)  strcat(str, "/String");
  if (type & number)  strcat(str, "/Number");
  if (type & list)    strcat(str, "/List");
  if (type & block)   strcat(str, "/Block");
  if (type & record)  strcat(str, "/Record");
  if (type & symbol)  strcat(str, "/Symbol");
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

static _Bool compare_records(RECORD rec1, RECORD rec2)
{
  if (rec1->len != rec2->len) return 0;
  size_t len = rec1->len;
  for (size_t i = 0; i < len; ++i)
  {
    SYMBOL name = rec1->items[i].name;
    size_t index1 = i;
    size_t index2 = 0;
    if (name == rec2->items[i].name) index2 = i;
    else
    {
      _Bool found = 0;
      for (size_t ii = 0; ii < len; ++ii)
      {
        const _Bool eq = name == rec2->items[ii].name;
        found |= eq;
        index2 += ii * eq;
      }
      if (!found) return 0;
    }
    if (!compare_objects(rec1->items[index1].object, rec2->items[index2].object))
      return 0;
  }
  return 1;
}

_Bool compare_objects(ANY ob1, ANY ob2)
{
  if (!(ob1.type & ob2.type))
  {
    return 0; // Not equal if differing types. None of this javascript rubbish.
  }
  switch (ob1.type)
  {
    case number  : return fabs(ob1.number - ob2.number) <= 0.5e-14 * fabs(ob1.number);
    case boolean : return ob1.boolean == ob2.boolean;
    case string  : return strcmp(ob1.string, ob2.string) == 0;
    case symbol  : return ob1.symbol == ob2.symbol;
    case list    : return compare_lists(ob1.list, ob2.list);
    case block   : throw_error("cannot compare blocks");
    case record  : return compare_records(ob1.record, ob2.record);
    case NOTHING : __builtin_trap();
  }
}

void * _NSConcreteStackBlock       [32] = { 0 };
void * _NSConcreteMallocBlock      [32] = { 0 };
void * _NSConcreteAutoBlock        [32] = { 0 };
void * _NSConcreteFinalizingBlock  [32] = { 0 };
void * _NSConcreteGlobalBlock      [32] = { 0 };
void * _NSConcreteWeakBlockVariable[32] = { 0 };

void *Block_copy(const void *arg) {
  struct Block_layout *aBlock = (struct Block_layout *)arg;
  struct Block_layout *result = GC_MALLOC(aBlock->descriptor->size);
  if unlikely(!result) return NULL;
  memcpy(result, aBlock, aBlock->descriptor->size);
  return result;
}

void _Block_object_assign(void *destAddr, const void *object, __attribute__((unused)) const int flags) {
  *(void**)destAddr = (void*)object;
}

void _Block_object_dispose(const void __attribute__((unused)) *object, const int __attribute__((unused)) flags) {}
