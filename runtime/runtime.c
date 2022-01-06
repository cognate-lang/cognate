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
#include <threads.h>
#include <sys/mman.h>

static const char *lookup_type(cognate_type);
static _Bool compare_lists(LIST, LIST);
static _Bool compare_groups(GROUP, GROUP);
static void handle_error_signal(int);

thread_local cognate_stack stack;
LIST cmdline_parameters = NULL;

const char* restrict word_name = NULL;
int line_num = -1;

thread_local const char* restrict function_stack_top;
thread_local const char* restrict function_stack_start;
ptrdiff_t function_stack_size;


void init(int argc, char** argv)
{
  struct rlimit stack_limit;
  if unlikely(getrlimit(RLIMIT_STACK, &stack_limit) == -1)
    throw_error("cannot get return stack limit");
  function_stack_size = stack_limit.rlim_cur;
  SET_FUNCTION_STACK_START();
  // Set locale for strings.
  if unlikely(setlocale(LC_ALL, "") == NULL)
  {
    throw_error("cannot set locale");
  }
  // Init GC
  gc_init();
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
    cognate_list* const tmp = gc_new (cognate_list);
    tmp->object = box_string(argv[argc]);
    tmp->next = cmdline_parameters;
    cmdline_parameters = tmp;
  }
  // Bind error signals.
  char signals[] = { SIGHUP, SIGSEGV, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGFPE, SIGPIPE, SIGTERM, SIGCHLD };
  for (size_t i = 0; i < sizeof(signals); ++i) signal(signals[i], handle_error_signal);
  // Initialize the stack.
  init_stack();
}
void cleanup(void)
{
  if unlikely(stack.top != stack.start || stack.cache != NIL_OBJ)
  {
    word_name = NULL;
    throw_error_fmt("exiting with %ti object(s) on the stack", stack.top - stack.start + (stack.cache != NIL_OBJ));
  }
}

void check_function_stack_size(void)
{
  const char sp;
  if unlikely(&sp < function_stack_top + STACK_MARGIN_KB * 1024)
    throw_error_fmt("maximum recursion depth exceeded");
}

void set_word_name(const char* restrict const name) { word_name=name; } // Need this to avoid unsequenced evaluation error.
void set_line_num(int num) { line_num=num; } // Need this to avoid unsequenced evaluation error.

_Noreturn __attribute__((format(printf, 1, 2))) void throw_error_fmt(const char* restrict const fmt, ...)
{
  const _Bool debug = word_name && line_num != -1;
  int offset = -2;
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
  if (errno) {
    const char* str = strerror(errno);
    fprintf(stderr, "%*s\033[0;2m%c%s\n", offset + 2, "", tolower(*str), str+1);
  }
  fputs("\033[0m", stderr);
  exit(EXIT_FAILURE);
}

_Noreturn void throw_error(const char* restrict const msg)
{
  const _Bool debug = word_name && line_num != -1;
  int offset = -2;
  fputc('\n', stderr);
  if (debug)
  {
    int line_num_digits = 1;
    for (int tmp = line_num; tmp /= 10; ++line_num_digits);
    offset = strlen("Line: ... ") + line_num_digits + strlen(word_name);
    fprintf(stderr, "\033[0;2mLine %i: \033[0;1m... %c%s ...\n%*s\033[31;1m↳ ",
        line_num, toupper(*word_name), word_name + 1, offset, "");
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

char* show_object (const ANY object, const _Bool raw_strings)
{
  // Virtual memory vastly simplfies this function.
  static char* buffer = NULL;
  if (!buffer)
    buffer = mmap(0, 1024l*1024l*1024l*1024l, PROT_READ | PROT_WRITE,
        MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
  char* buffer_start = buffer;
  switch (get_type(object))
  {
    case number: sprintf(buffer, "%.14g", unbox_number(object)); break;
    case string: if (raw_strings) strcpy(buffer, unbox_string(object));
                 else {
                   *buffer++ = '\'';
                   for (const char* str = unbox_string(object) ; *str ; ++str)
                   {
                     char c = *str;
                     if unlikely(c >= '\a' && c <= '\r')
                     {
                       *buffer++ = '\\';
                       *buffer++ = "abtnvfr"[c-'\a'];
                     }
                     else if (c == '\\') { *buffer++ = '\\'; *buffer++ = '\\'; }
                     else if (c == '\'') { *buffer++ = '\\'; *buffer++ = '\''; }
                     else *(buffer++) = c;
                   }
                   *buffer++ = '\'';
                   *buffer++ = '\0';
                 }
                 break;
    case list: *buffer++ = '(';
               for (LIST l = unbox_list(object) ;; l = l->next)
               {
                 buffer += strlen(show_object(l->object, 0));
                 if (!l->next) break;
                 *buffer++ = ',';
                 *buffer++ = ' ';
               }
               *buffer++ = ')';
               *buffer++ = '\0';
               break;
    case group: {
                  GROUP g = unbox_group(object);
                  for (size_t i = 0 ;; ++i)
                  {
                    buffer += strlen(strcpy(buffer, g->items[i].name));
                    *buffer++ = ':';
                    *buffer++ = ' ';
                    buffer += strlen(show_object(g->items[i].object, 0));
                    if (i + 1 == g->len) break;
                    *buffer++ = ',';
                    *buffer++ = ' ';
                  }
                  *buffer++ = ')';
                  *buffer++ = '\0';
                }
                break;
    case boolean: strcpy(buffer, unbox_boolean(object) ? "True" : "False");  break;
    case symbol:  strcpy(buffer, unbox_symbol(object));                      break;
    case block:   sprintf(buffer, "<block %p>", (void*)unbox_block(object)); break;
    case NOTHING: __builtin_trap();
  }
  buffer = buffer_start;
  return buffer;
}

void init_stack(void)
{
  stack.size = INITIAL_STACK_SIZE;
  stack.top = stack.start = gc_malloc(INITIAL_STACK_SIZE);
  stack.cache = NIL_OBJ;
}

void push(ANY object)
{
  if likely(stack.cache == NIL_OBJ) { stack.cache = object; return; }
  if unlikely(stack.start + stack.size == stack.top) expand_stack();
  *stack.top++ = stack.cache;
  stack.cache = object;
}

ANY pop(void)
{
  if likely(stack.cache != NIL_OBJ) { const ANY a = stack.cache; stack.cache = NIL_OBJ; return a; }
  if unlikely(stack.top == stack.start) throw_error("stack underflow");
  return *--stack.top;
}

ANY peek(void)
{
  if likely(stack.cache != NIL_OBJ) return stack.cache;
  if unlikely(stack.top == stack.start) throw_error("stack underflow");
  return *(stack.top - 1);
}

void flush_stack_cache(void)
{
  if (stack.cache == NIL_OBJ) return;
  push(stack.cache);
  pop();
}

int stack_length(void)
{
  return stack.top - stack.start + (stack.cache != NIL_OBJ);
}

void expand_stack(void)
{
  // Assumes that stack is currently of length stack.size.
  size_t new_size = stack.size * LIST_GROWTH_FACTOR;
  stack.start = memcpy(gc_malloc(new_size * sizeof(ANY)), stack.start, new_size);
  stack.top = stack.start + stack.size;
  stack.size *= LIST_GROWTH_FACTOR;
}

const char* lookup_type(cognate_type type)
{
  switch(type)
  {
    case NOTHING: return "NOTHING";
    case string:  return "string";
    case number:  return "number";
    case list:    return "list";
    case block:   return "block";
    case group:   return "group";
    case symbol:  return "symbol";
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
  return (signed long)box > (signed long)NAN_MASK;
}

cognate_type get_type(ANY box)
{
  if (is_nan(box)) return (TYP_MASK & box) >> 48;
  else return number;
}

NUMBER unbox_number(ANY box)
{
  if unlikely(is_nan(box))
    throw_error_fmt("expected a Number but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
  return *(NUMBER*)&box;
}

ANY box_number(NUMBER num)
{
  return *(ANY*)&num;
}

BOOLEAN unbox_boolean(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)boolean << 48)
    throw_error_fmt("expected a Boolean but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
  return (STRING)(PTR_MASK & box);
}

ANY box_boolean(BOOLEAN b)
{
  return NAN_MASK | ((long)boolean << 48) | b;
}

STRING unbox_string(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)string << 48)
    throw_error_fmt("expected a String but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
  return (STRING)(PTR_MASK & box);
}

ANY box_string(STRING s)
{
  return NAN_MASK | ((long)string << 48) | (long)s;
}

LIST unbox_list(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)list << 48)
    throw_error_fmt("expected a List but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
  return (LIST)(PTR_MASK & box);
}

ANY box_list(LIST s)
{
  return NAN_MASK | ((long)list << 48) | (long)s;
}

GROUP unbox_group(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)group << 48)
    throw_error_fmt("expected a Group but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
  return (GROUP)(PTR_MASK & box);
}

ANY box_group(GROUP s)
{
  return NAN_MASK | ((long)group << 48) | (long)s;
}

SYMBOL unbox_symbol(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)symbol << 48)
    throw_error_fmt("expected a Symbol but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
  return (SYMBOL)(PTR_MASK & box);
}

ANY box_symbol(SYMBOL s)
{
  return NAN_MASK | ((long)symbol << 48) | (long)s;
}

BLOCK unbox_block(ANY box)
{
  if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)block << 48)
    throw_error_fmt("expected a Block but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
  return (BLOCK)(PTR_MASK & box);
}

ANY box_block(BLOCK s)
{
  return NAN_MASK | ((long)block << 48) | (long)Block_copy(s);
}
