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
static _Bool compare_groups(GROUP, GROUP);
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
    tmp->object = box_string(argv[argc]);
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
  if unlikely(stack.top != stack.start || stack.cache != NIL_OBJ)
  {
    word_name = NULL;
    throw_error_fmt("exiting with %ti object(s) on the stack", stack.top - stack.start + (stack.cache != NIL_OBJ));
  }
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
  switch (get_type(object))
  {
    // Double precision float has 15sf precision.
    // Switch to scientific notation after 10 digits to eliminate rounding errors.
    case number: fprintf(out, "%.14g", unbox_number(object));  return;
    case string:
    {
      // Quotes is whether or not to print strings with quotes.
      if (!quotes)
      {
        fputs(unbox_string(object), out);
        return;
      }
      fputc('\'', out);
      char c;
      for (const char* restrict ptr = unbox_string(object); (c = *ptr) != '\0'; ++ptr)
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
      for (LIST i = unbox_list(object); i ; i = i->next)
      {
        print_object(i->object, out, 1);
        if likely(i->next) fputc(' ', out);
      }
      fputc(')', out);
      return;
    }
    case group:
    {
      GROUP g = unbox_group(object);
      fputc('(', out);
      for (size_t i = 0; i < g->len; ++i)
      {
        fprintf(out, "%s: ", g->items[i].name);
        print_object(g->items[i].object, out, 1);
        if likely(i + 1 < g->len) fputs(", ", out);
      }
      fputc(')', out);
      return;
    }
    case boolean: fputs(unbox_boolean(object) ? "True" : "False", out); return;
    case block: fprintf(out, "<Block %p>", (void*)unbox_block(object)); return;
    case symbol: fputs(unbox_symbol(object), out); return;
    case NOTHING: __builtin_trap();
  }
}

void init_stack()
{
  stack.size = INITIAL_LIST_SIZE;
  stack.top = stack.start = GC_MALLOC (INITIAL_LIST_SIZE * sizeof(ANY));
  stack.cache = NIL_OBJ;
}

void push(ANY object)
{
  if likely(stack.cache == NIL_OBJ) { stack.cache = object; return; }
  if unlikely(stack.start + stack.size == stack.top) expand_stack();
  *stack.top++ = stack.cache;
  stack.cache = object;
}

ANY pop()
{
  if likely(stack.cache != NIL_OBJ) { const ANY a = stack.cache; stack.cache = NIL_OBJ; return a; }
  if unlikely(stack.top == stack.start) throw_error("stack underflow");
  return *--stack.top;
}

ANY peek()
{
  if likely(stack.cache != NIL_OBJ) return stack.cache;
  if unlikely(stack.top == stack.start) throw_error("stack underflow");
  return *(stack.top - 1);
}

int stack_length()
{
  return stack.top - stack.start + (stack.cache != NIL_OBJ);
}

void expand_stack()
{
  // Assumes that stack is currently of length stack.size.
  stack.start = (ANY*) GC_REALLOC (stack.start, stack.size * LIST_GROWTH_FACTOR * sizeof(ANY));
  stack.top = stack.start + stack.size;
  stack.size *= LIST_GROWTH_FACTOR;
}

const char* lookup_type(cognate_type type)
{
  switch(type)
  {
    case NOTHING: return "NOTHING";
    case string:  return "String";
    case number:  return "Number";
    case list:    return "List";
    case block:   return "Block";
    case group:   return "Group";
    case symbol:  return "Symbol";
    default: __builtin_trap();
  }
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

static _Bool compare_groups(GROUP g1, GROUP g2)
{
  if (g1->len != g2->len) return 0;
  size_t len = g1->len;
  for (size_t i = 0; i < len; ++i)
  {
    SYMBOL name = g1->items[i].name;
    size_t index1 = i;
    size_t index2 = 0;
    if (name == g2->items[i].name) index2 = i;
    else
    {
      _Bool found = 0;
      for (size_t ii = 0; ii < len; ++ii)
      {
        const _Bool eq = name == g2->items[ii].name;
        found |= eq;
        index2 += ii * eq;
      }
      if (!found) return 0;
    }
    if (!compare_objects(g1->items[index1].object, g2->items[index2].object))
      return 0;
  }
  return 1;
}

_Bool compare_objects(ANY ob1, ANY ob2)
{
  if (!(get_type(ob1) == get_type(ob2)))
  {
    return 0; // Not equal if differing types. None of this javascript rubbish.
  }
  switch (get_type(ob1))
  {
    case number  : return fabs(unbox_number(ob1) - unbox_number(ob2)) <= 0.5e-14 * fabs(unbox_number(ob1));
    case boolean : return unbox_boolean(ob1) == unbox_boolean(ob2);
    case string  : return strcmp(unbox_string(ob1), unbox_string(ob2)) == 0;
    case symbol  : return unbox_symbol(ob1) == unbox_symbol(ob2);
    case list    : return compare_lists(unbox_list(ob1), unbox_list(ob2));
    case block   : throw_error("cannot compare blocks");
    case group   : return compare_groups(unbox_group(ob1), unbox_group(ob2));
    default      : __builtin_trap();
  }
}

static _Bool is_nan(ANY box)
{
  // Works with -ffast-math
  return (box & NAN_MASK) == NAN_MASK;
}

cognate_type get_type(ANY box)
{
  if (is_nan(box)) return (TYP_MASK & box) >> 48;
  else return number;
}

NUMBER unbox_number(ANY box)
{
  if unlikely(is_nan(box))
    throw_error_fmt("expected Number got %s", lookup_type(get_type(box)));
  return *(NUMBER*)&box;
}

ANY box_number(NUMBER num)
{
  return *(ANY*)&num;
}

BOOLEAN unbox_boolean(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)boolean << 48)
    throw_error_fmt("expected Boolean got %s", lookup_type(get_type(box)));
  return (STRING)(PTR_MASK & box);
}

ANY box_boolean(BOOLEAN b)
{
  return NAN_MASK | ((long)boolean << 48) | b;
}

STRING unbox_string(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)string << 48)
    throw_error_fmt("expected String got %s", lookup_type(get_type(box)));
  return (STRING)(PTR_MASK & box);
}

ANY box_string(STRING s)
{
  return NAN_MASK | ((long)string << 48) | (long)s;
}

LIST unbox_list(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)list << 48)
    throw_error_fmt("expected List got %s", lookup_type(get_type(box)));
  return (LIST)(PTR_MASK & box);
}

ANY box_list(LIST s)
{
  return NAN_MASK | ((long)list << 48) | (long)s;
}

GROUP unbox_group(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)group << 48)
    throw_error_fmt("expected Group got %s", lookup_type(get_type(box)));
  return (GROUP)(PTR_MASK & box);
}

ANY box_group(GROUP s)
{
  return NAN_MASK | ((long)group << 48) | (long)s;
}

SYMBOL unbox_symbol(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)symbol << 48)
    throw_error_fmt("expected Symbol got %s", lookup_type(get_type(box)));
  return (SYMBOL)(PTR_MASK & box);
}

ANY box_symbol(SYMBOL s)
{
  return NAN_MASK | ((long)symbol << 48) | (long)s;
}

BLOCK unbox_block(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)block << 48)
    throw_error_fmt("expected Block got %s", lookup_type(get_type(box)));
  return (BLOCK)(PTR_MASK & box);
}

ANY box_block(BLOCK s)
{
  return NAN_MASK | ((long)block << 48) | (long)Block_copy(s);
}
