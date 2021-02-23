#ifndef FUNC_C
#define FUNC_C

#include "cognate.h"
#include "types.h"

static cognate_list params = NULL;

// I'm not putting type signatures for every single function here.

#include "type.c"
#include "stack.c"
#include "table.c"
#include "io.c"

#include <unistd.h>
#include <regex.h>
#include <math.h>
#include <string.h>
#include <limits.h>
#include <stdlib.h>
#ifndef noGC
#include <gc/gc.h>
#endif

static void ___if(cognate_block cond, cognate_object a, cognate_object b)
{
  cond();
  if (CHECK(boolean, pop()))
    push(a);
  else
    push(b);
}

static void ___while(cognate_block cond, cognate_block body) {
  cond();
  while (CHECK(boolean, pop()))
  {
    body();
    cond();
  }
}

static void ___do(cognate_block blk) { blk(); }

static void ___put(cognate_object a)   { print_object(a, stdout, 0); fflush(stdout); }
static void ___print(cognate_object a) { print_object(a, stdout, 0); puts(""); }


static cognate_number ___sum(cognate_number a, cognate_number b)      { return a + b; }
static cognate_number ___multiply(cognate_number a, cognate_number b) { return a * b; }
static cognate_number ___subtract(cognate_number a, cognate_number b) { return b - a; }
static cognate_number ___divide(cognate_number a, cognate_number b)   { if likely(a) return b / a; throw_error("Division of %.14g by zero", b); }

static cognate_number ___modulo(cognate_number a, cognate_number b) {
  if likely(a) return fmod(b, a);
  throw_error("Modulo of %.14g by zero", b);
}

static cognate_number ___random(cognate_number low, cognate_number high, cognate_number step) {
  if unlikely((high - low) * step < 0 || !step)
  {
    throw_error("Invalid range %.14g..%.14g with step %.14g", low, high, step);
  }
  else if ((high - low) / step < 1)
  {
    return low;
  }
  // This is not cryptographically secure btw.
  // Since RAND_MAX may only be 2^15, we need to do this:
  const long num
    = ((long)(short)random())
    | ((long)(short)random() << 15)
    | ((long)(short)random() << 30)
    | ((long)(short)random() << 45)
    | ((long)       random() << 60);
  return low + (cognate_number)(num % (unsigned long)((high - low) / step)) * step;
}

static void ___drop(cognate_object a)                   { (void)a; } // These can be defined within cognate.
static void ___twin(cognate_object a)                   { push(a); push(a); }
static void ___triplet(cognate_object a)                { push(a); push(a); push(a); }
static void ___swap(cognate_object a, cognate_object b) { push(a); push(b); }
static void ___clear()                                  { stack.top=stack.start; }

static cognate_boolean ___true()  { return 1; }
static cognate_boolean ___false() { return 0; }

static cognate_boolean ___either(cognate_boolean a, cognate_boolean b) { return(a || b); } // Use unconventional operators to avoid short-circuits.
static cognate_boolean ___both  (cognate_boolean a, cognate_boolean b) { return(a && b); }
static cognate_boolean ___one_of(cognate_boolean a, cognate_boolean b) { return(a ^ b); }
static cognate_boolean ___not   (cognate_boolean a)                    { return !a; }


static cognate_boolean ___equal(cognate_object a, cognate_object b)   { return compare_objects(a,b); }
static cognate_boolean ___unequal(cognate_object a, cognate_object b) { return !compare_objects(a,b); }
static cognate_boolean ___exceed(cognate_number a, cognate_number b)  { return a < b; }
static cognate_boolean ___preceed(cognate_number a, cognate_number b) { return a > b; }
static cognate_boolean ___equalorpreceed(cognate_number a, cognate_number b) { return a >= b; }
static cognate_boolean ___equalorexceed(cognate_number a, cognate_number b)  { return a <= b; }

static cognate_boolean ___number_(cognate_object a)  { return a.type&number; } // Question marks are converted to underscores.
static cognate_boolean ___list_(cognate_object a)    { return a.type&number; } // However all other symbols are too.
static cognate_boolean ___string_(cognate_object a)  { return a.type&string; } // So this is a temporary hack!
static cognate_boolean ___block_(cognate_object a)   { return a.type&block;  }
static cognate_boolean ___boolean_(cognate_object a) { return a.type&boolean;}

static void ___first(cognate_list lst) {
  // Returns the first element of a list. O(1).
  if unlikely(!lst) throw_error("Empty list is invalid");
  push(lst->object);
}

static cognate_list ___rest(cognate_list lst) {
  // Returns the tail portion of a list. O(1).
  if unlikely(!lst) throw_error("Empty list is invalid");
  return lst->next;
}

static cognate_string ___head(cognate_string str)
{
  if unlikely(!*str) throw_error("Empty string is invalid");
  return GC_STRNDUP(str, mblen(str, MB_CUR_MAX));
}

static cognate_string ___tail(cognate_string str)
{
  if unlikely(!*str) throw_error("Empty string is invalid");
  return str + mblen(str, MB_CUR_MAX);
}

static cognate_list ___push(cognate_object a, cognate_list b) {
  // Pushes an object from the stack onto the list's first element. O(1).
  // TODO: Better name? Inconsistent with List where pushing to the stack adds to the END.
  cognate_list_node* lst = GC_NEW (cognate_list_node);
  lst->object = a;
  lst->next   = b;
  return lst;
}

static cognate_boolean ___empty_(cognate_object container) {
  // Returns true is a list or string is empty. O(1).
  // Can be used to to write a Length function.
  return check_type(list | string, container).type == list ? !container.list : *container.string == '\0';
}

static cognate_list ___list(cognate_block expr) {
  // Move the stack to temporary storage
  const cognate_stack temp_stack = stack;
  // Allocate a list as the stack
  init_stack();
  // Eval expr
  expr();
  // Move to a list.
  cognate_list lst = NULL;
  while (stack.top != stack.start)
  {
    // This can just be Swap, Push;
    cognate_list_node* tmp = GC_NEW (cognate_list_node);
    tmp -> object = pop();
    tmp -> next = lst;
    lst = tmp;
  }
  // Restore the stack.
  stack = temp_stack;
  return lst;
}

static void ___characters() {
  // Can be rewritten using substring, albeit O(n^2)
  /*
  const char* str = pop(string);
  const cognate_list* lst = NULL;
  for (size_t i = 0; *str ; ++i)
  {
    size_t char_len = mblen(str, MB_CUR_MAX);
    cognate_list* tmp = GC_NEW (cognate_list);
    tmp->object = (cognate_object) {.type=string, .string=GC_STRNDUP(str, char_len)};
    tmp->next = lst;
    lst = tmp;
    str += char_len;
  }
  push(list, lst);
  */ // TODO: Currently returning list backwards.
}

static void ___split() {
  // Can be rewritten using Substring.
  /*
  const char* const delimiter = pop(string);
  const size_t delim_size     = strlen(delimiter);
  if unlikely(!delim_size) throw_error("Cannot Split a string with a zero-length delimiter!");
  const char* str = pop(string);
  size_t length = 1;
    for (const char* temp = str; (temp = strstr(temp, delimiter) + delim_size) - delim_size; ++length);
  cognate_list* const lst = GC_NEW(cognate_list);
  lst->top = lst->start   = (cognate_object*) GC_MALLOC (sizeof(cognate_object) * length);
  lst->top += length;
  size_t i = 0;
  for (const char* c; (c = strstr(str, delimiter)); i++)
  {
    lst->start[i] = (cognate_object) {.type=string, .string=GC_STRNDUP(str, c - str)};
    str = c + delim_size;
  }
  lst->start[i] = (cognate_object) {.type=string, .string=str};
  push(list, lst);
  */ // TODO
}

static cognate_string ___join(cognate_number n) {
  // Joins a string to the end of another string.
  // Define Prefix (Swap, Suffix);
  size_t n1 = n;
  if (n != n1) throw_error("Cannot join %.14g strings", n);
  const char* strings[n1];
  size_t result_size = 1;
  for (size_t i = 0; i < n1; ++i)
  {
    const char* str = check_type(string, pop()).string;
    strings[i] = str;
    result_size += strlen(str);
  }
  char* result = GC_MALLOC(result_size);
  result[0] = '\0';
  for (size_t i = 0; i < n1; ++i)
  {
    strcat(result, strings[i]);
  }
  return result;
}

static cognate_number ___string_length(cognate_string str) {
  return mbstrlen(str);
}

static cognate_string ___substring(cognate_number startf, cognate_number endf, cognate_string str) {
  const char* str_start = str;
  // O(end).
  // Only allocates a new string if it has to.
  /* TODO: Would it be better to have a simpler and more minimalist set of string functions, like lists do?
   * The only real difference between NULL terminated strings and linked lists is that appending to strings is harder.
   * Maybe also a 'Join N Str1 Str2 Str3 ... StrN' function.
   */
  size_t start  = startf;
  size_t end    = endf;
  if unlikely(start != startf || end != endf || start > end)
    throw_error("Invalid range %.14g..%.14g for string %.64s", startf, endf, str_start);
  size_t str_size = 0;
  end -= start;
  for (;start != 0; --start)
  {
    if unlikely(!*str) throw_error("String %.64s is too small (%li characters)", str_start, mbstrlen(str_start)); // TODO Show more info here.
    str += mblen(str, MB_CUR_MAX);
  }
  for (;end != 0; --end)
  {
    if unlikely(str[str_size] == '\0') throw_error("String %.64s is too small (%li characters)", str_start, mbstrlen(str_start));
    str_size += mblen(str+str_size, MB_CUR_MAX);
  }
  if unlikely(str[str_size] == '\0')
  {
    // We don't need to make a new string here.
    return str;
  }
  return GC_STRNDUP(str, str_size);
}


static cognate_string ___input() {
  // Read user input to a string.
  size_t size = 0;
  char* buf;
  size_t chars = getline(&buf, &size, stdin);
  char* ret = GC_STRNDUP(buf, chars-1); // Don't copy trailing newline.
  free(buf);
  return ret;
}

static cognate_string ___read(cognate_string filename) {
  // Read a file to a string.
  FILE *fp = fopen(filename, "ro");
  if unlikely(fp == NULL) throw_error("Cannot open file '%s'. It probably doesn't exist.", filename);
  fseek(fp, 0L, SEEK_END);
  size_t file_size = ftell(fp);
  rewind(fp);
  char* text = GC_MALLOC (file_size);
  fread(text, sizeof(char), file_size, fp);
  fclose(fp);
  text[file_size-1] = '\0'; // Remove trailing eof.
  return text;
  // TODO: single line (or delimited) file read function for better IO performance?
}

static cognate_number ___number(cognate_string str) {
  // casts string to number.
  char* end;
  cognate_number num = strtod(str, &end);
  if (end == str || *end != '\0')
  {
    throw_error("Cannot parse '%.64s' to a number", str);
  }
  return num;
}

static cognate_string ___path() {
  char buf[FILENAME_MAX];
  if (!getcwd(buf, FILENAME_MAX))
  {
    throw_error("Cannot get file path");
  }
  char* ret = GC_STRDUP(buf);
  return ret;
}

static void ___stack() {
  // We can't return the list or this function is inlined and it breaks.
  copy_blocks();
  cognate_list lst = NULL;
  for (cognate_object* i = stack.top - 1 ; i >= stack.start ; --i)
  {
    // TODO: Allocate the list as an array for the sake of memory locality.
    cognate_list_node* tmp = GC_NEW (cognate_list_node);
    tmp -> object = *i;
    tmp -> next = lst;
    lst = tmp;
  }
  push(OBJ(list, lst));
}

static void ___write(cognate_string filename, cognate_object obj) {
  // Write object to end of file, without a newline.
  FILE* const fp = fopen(filename, "a");
  if unlikely(fp == NULL) throw_error("Cannot open file '%s'. It probably doesn't exist.", filename);
  print_object(obj, fp, 0);
  fclose(fp);
}

static cognate_list ___parameters() {
  return params;
}

static void ___stop() {
  // Don't check stack length, because it probably wont be empty.
  exit(0);
}

static const cognate_table* ___table() {
  /*
  ___list();
  const cognate_list init = *pop(list);
  const size_t table_size = ((init.top - init.start) * LIST_GROWTH_FACTOR);
  cognate_table* const tab = GC_NEW(cognate_table); // Need to allocate list here.
  tab->items.start = (cognate_object*) GC_MALLOC (sizeof(cognate_object) * table_size);
  tab->items.top = tab->items.start + table_size;
  tab->confirmation_hash = (unsigned long*) GC_MALLOC (sizeof(unsigned long) * table_size);
  const char *key;
  cognate_object value;
  for (const cognate_object *i = init.start + 1; i < init.top; i += 2)
  {
    key = check_type(string, *i).string;
    value = *(i-1);
    *tab = table_add(hash(key), value, *tab);
  }
  push(table, tab);
  */ // TODO
  return NULL;
}

static const cognate_table* ___insert(cognate_string key, cognate_object value, cognate_table tab) {
  // O(n) :(
  //*tab = table_add(hash(key), value, table_copy(*pop(table)));
  //push(table, tab);
  (void)key;
  (void)value;
  (void)tab;
  return NULL;
}

static void ___get(cognate_string key, cognate_table tab) {
  // O(1) mostly;
  /*
  const char* const key = pop(string);
  const cognate_table tab = *pop(table);
  push(table_get(key, tab));
  */
  (void)key;
  (void)tab;
}

static cognate_list ___values(cognate_table tab) {
  (void)tab;
  // O(n)
  // Resulting list is NOT in any order at all.
  // Equivilant tables may give differently ordered lists.
  /*
  const cognate_table tab = *pop(table);
  cognate_list* const lst = GC_NEW(cognate_list);
  const long table_size = tab.items.top - tab.items.start;
  lst->start = (cognate_object*) GC_MALLOC (sizeof(cognate_object) * table_size);
  int j = 0;
  for (int i = 0; i < table_size; ++i)
  {
    if (tab.items.start[i].type != NOTHING)
    {
      lst->start[j++] = tab.items.start[i];
    }
  }
  lst->top = lst->start + j;
  push(list, lst);
  */ // TODO
  return NULL;
}

static cognate_boolean ___match(cognate_string reg_str, cognate_string str) {
  // Returns true if string matches regex.
  static const char* old_str = NULL;
  static regex_t reg;
  if (old_str == NULL || strcmp(reg_str, old_str) != 0)
  {
    // Technically, the last regex to be used in the program will leak memory.
    // However, this is minor, since only a limited amount of memory can be leaked.
    regfree(&reg); // Apparently freeing an unallocated regex is fine.
    if unlikely(!*reg_str) throw_error("Cannot compile empty string as regex");
    const int status = regcomp(&reg, reg_str, REG_EXTENDED | REG_NEWLINE | REG_NOSUB);
    if unlikely(status)
    {
      char reg_err[256];
      regerror(status, &reg, reg_err, 256);
      throw_error("Regex compile error: %s in regex '%.32s'", reg_err, reg_str);
    }
    old_str = reg_str; /* This should probably be strcpy, but I trust that reg_str is either
                          allocated with the garbage collector, or read only in the data segment. */
  }
  const int found = regexec(&reg, str, 0, NULL, 0);
  if unlikely(found != 0 && found != REG_NOMATCH)
  {

    throw_error("Regex match error on string '%.64s' with regex '%.64s'", str, reg_str);
    // If this error ever actually appears, use regerror to get the full text.
  }
  return !found;
}

static cognate_number ___ordinal(cognate_string str) {
  if unlikely(mbstrlen(str) != 1)
  {
    throw_error("Invalid string '%.64s' (should be length 1)", str);
  }
  int chr = 0;
  mbtowc(&chr, str, MB_CUR_MAX);
  return chr;
}

static cognate_string ___character(cognate_number d) {
  const int i = d;
  if unlikely(i != d) throw_error("Cannot convert %.14g to UTF8 character", d);
  char* str = GC_MALLOC (MB_CUR_MAX + 1);
  wctomb(str, i);
  str[mblen(str, MB_CUR_MAX)] = '\0';
  return str;
}

static cognate_number ___floor(cognate_number a) {
  return floor(a);
}

static cognate_number ___round(cognate_number a) {
  return round(a);
}

static cognate_number ___ceiling(cognate_number a) {
  return ceil(a);
}

static void ___assert(cognate_string name, cognate_boolean result) {
  if unlikely(!result)
  {
    throw_error("Assertion '%s' has failed", name);
  }
}

static void ___error(cognate_string str) {
  //word_name = NULL;
  //errno = 0;
  throw_error("%s", str);
}

#endif
