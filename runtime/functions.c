#include "runtime.h"

#include <errno.h>
#include <math.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#ifndef NO_GC
#include <gc/gc.h>
#endif

static size_t mbstrlen(const char* str);

ANY VAR(if)(BLOCK cond, cognate_object a, cognate_object b)
{
  cond();
  if (CHECK(boolean, pop()))
    return a;
  else
    return b;
}

void VAR(while)(BLOCK cond, BLOCK body) {
  cond();
  while (CHECK(boolean, pop()))
  {
    body();
    cond();
  }
}

void VAR(do)(BLOCK blk) { blk(); }

void VAR(put)(cognate_object a)   { print_object(a, stdout, 0); fflush(stdout); }
void VAR(print)(cognate_object a) { print_object(a, stdout, 0); putc('\n', stdout); }


NUMBER VAR(ADD)(NUMBER a, NUMBER b)      { return a + b; }
NUMBER VAR(MUL)(NUMBER a, NUMBER b) { return a * b; }
NUMBER VAR(SUB)(NUMBER a, NUMBER b) { return b - a; }
NUMBER VAR(DIV)(NUMBER a, NUMBER b)   { if likely(a) return b / a; throw_error("Division of %.14g by zero", b); }

NUMBER VAR(modulo)(NUMBER a, NUMBER b) {
  if likely(a) return fmod(b, a);
  throw_error("Modulo of %.14g by zero", b);
}

NUMBER VAR(random)(NUMBER low, NUMBER high, NUMBER step) {
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
    = ((long)(short)rand())
    | ((long)(short)rand() << 15)
    | ((long)(short)rand() << 30)
    | ((long)(short)rand() << 45)
    | ((long)       rand() << 60);
  return low + (NUMBER)(num % (unsigned long)((high - low) / step)) * step;
}

void VAR(clear)() { stack.top=stack.start; }

ANY VAR(true) = OBJ(boolean,1);
ANY VAR(false) = OBJ(boolean,0);

BOOLEAN VAR(either)(BOOLEAN a, BOOLEAN b) { return a || b; }
BOOLEAN VAR(both)(BOOLEAN a, BOOLEAN b) { return a && b; }
BOOLEAN VAR(one_of)(BOOLEAN a, BOOLEAN b) { return a ^ b;  }
BOOLEAN VAR(not)(BOOLEAN a) { return !a;     }


BOOLEAN VAR(EQ)(cognate_object a, cognate_object b)  { return compare_objects(a,b); }
BOOLEAN VAR(NEQ)(cognate_object a, cognate_object b) { return !compare_objects(a,b); }
BOOLEAN VAR(GT)(NUMBER a, NUMBER b)  { return a < b; }
BOOLEAN VAR(LT)(NUMBER a, NUMBER b)  { return a > b; }
BOOLEAN VAR(GTE)(NUMBER a, NUMBER b) { return a >= b; }
BOOLEAN VAR(LTE)(NUMBER a, NUMBER b) { return a <= b; }

BOOLEAN VAR(number_)(cognate_object a)  { return a.type&number; } // Question marks are converted to underscores.
BOOLEAN VAR(list_)(cognate_object a)    { return a.type&number; } // However all other symbols are too.
BOOLEAN VAR(string_)(cognate_object a)  { return a.type&string; } // So this is a temporary hack!
BOOLEAN VAR(block_)(cognate_object a)   { return a.type&block;  }
BOOLEAN VAR(boolean_)(cognate_object a) { return a.type&boolean;}

ANY VAR(first)(LIST lst)
{
  // Returns the first element of a list. O(1).
  if unlikely(!lst) throw_error("Empty list is invalid");
  return lst->object;
}

LIST VAR(rest)(LIST lst)
{
  // Returns the tail portion of a list. O(1).
  if unlikely(!lst) throw_error("Empty list is invalid");
  return lst->next;
}

STRING VAR(head)(STRING str)
{
  if unlikely(!*str) throw_error("Empty string is invalid");
  return GC_STRNDUP(str, mblen(str, MB_CUR_MAX));
}

STRING VAR(tail)(STRING str)
{
  if unlikely(!*str) throw_error("Empty string is invalid");
  return str + mblen(str, MB_CUR_MAX);
}

LIST VAR(push)(cognate_object a, LIST b) {
  // Pushes an object from the stack onto the list's first element. O(1).
  // TODO: Better name? Inconsistent with List where pushing to the stack adds to the END.
  cognate_list* lst = GC_NEW (cognate_list);
  lst->object = a;
  lst->next   = b;
  return lst;
}

BOOLEAN VAR(empty_)(LIST lst) {
  // Returns true is a list or string is empty. O(1).
  // Can be used to to write a Length function.
  return !lst;
}

LIST VAR(list)(BLOCK expr) {
  // Move the stack to temporary storage
  const cognate_stack temp_stack = stack;
  // Allocate a list as the stack
  init_stack();
  // Eval expr
  expr();
  // Move to a list.
  LIST lst = NULL;
  while (stack.top != stack.start)
  {
    // This can just be Swap, Push;
    cognate_list* tmp = GC_NEW (cognate_list);
    tmp -> object = pop();
    tmp -> next = lst;
    lst = tmp;
  }
  // Restore the stack.
  stack = temp_stack;
  return lst;
}

/*
LIST VAR(characters)() {
  // Can be rewritten using substring, albeit O(n^2)
  const char* str = pop(string);
  const LIST* lst = NULL;
  for (size_t i = 0; *str ; ++i)
  {
    size_t char_len = mblen(str, MB_CUR_MAX);
    LIST* tmp = GC_NEW (LIST);
    tmp->object = (cognate_object) {.type=string, .string=GC_STRNDUP(str, char_len)};
    tmp->next = lst;
    lst = tmp;
    str += char_len;
  }
  return NULL;
  // TODO: Currently returning list backwards.
}
*/
/*
LIST VAR(split)() {
  // Can be rewritten using Substring.
  const char* const delimiter = pop(string);
  const size_t delim_size     = strlen(delimiter);
  if unlikely(!delim_size) throw_error("Cannot Split a string with a zero-length delimiter!");
  const char* str = pop(string);
  size_t length = 1;
    for (const char* temp = str; (temp = strstr(temp, delimiter) + delim_size) - delim_size; ++length);
  LIST* const lst = GC_NEW(LIST);
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
  return NULL;
}
*/

STRING VAR(join)(NUMBER n) {
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

NUMBER VAR(string_length)(STRING str) {
  return mbstrlen(str);
}

STRING VAR(substring)(NUMBER startf, NUMBER endf, STRING str) {
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


STRING VAR(input)() {
  // Read user input to a string.
  size_t size = 0;
  char* buf;
  size_t chars = getline(&buf, &size, stdin);
  char* ret = GC_STRNDUP(buf, chars-1); // Don't copy trailing newline.
  free(buf);
  return ret;
}

STRING VAR(read)(STRING filename) {
  // Read a file to a string.
  FILE *fp = fopen(filename, "ro");
  if unlikely(fp == NULL) throw_error("Cannot open file '%s'. It probably doesn't exist.", filename);
  struct stat st;
  fstat(fileno(fp), &st);
  char* text = GC_MALLOC (st.st_size + 1);
  if (fread(text, sizeof(char), st.st_size, fp) != st.st_size) throw_error("Error reading file '%s'.", filename);
  fclose(fp);
  text[st.st_size-1] = '\0'; // Remove trailing eof.
  return text;
  // TODO: single line (or delimited) file read function for better IO performance?
}

NUMBER VAR(number)(STRING str) {
  // casts string to number.
  char* end;
  NUMBER num = strtod(str, &end);
  if (end == str || *end != '\0')
  {
    throw_error("Cannot parse '%.64s' to a number", str);
  }
  return num;
}

STRING VAR(path)() {
  char buf[FILENAME_MAX];
  if (!getcwd(buf, FILENAME_MAX))
  {
    throw_error("Cannot get file path");
  }
  char* ret = GC_STRDUP(buf);
  return ret;
}

LIST VAR(stack)() {
  // We can't return the list or this function is inlined and it breaks.
  LIST lst = NULL;
  for (cognate_object* i = stack.top - 1 ; i >= stack.start ; --i)
  {
    // TODO: Allocate the list as an array for the sake of memory locality.
    cognate_list* tmp = GC_NEW (cognate_list);
    tmp -> object = *i;
    tmp -> next = lst;
    lst = tmp;
  }
  return lst;
}

void VAR(write)(STRING filename, cognate_object obj) {
  // Write object to end of file, without a newline.
  FILE* const fp = fopen(filename, "a");
  if unlikely(fp == NULL) throw_error("Cannot open file '%s'. It probably doesn't exist.", filename);
  print_object(obj, fp, 0);
  fclose(fp);
}

LIST VAR(parameters)() {
  return cmdline_parameters; // TODO should be a variable, and allow mutation and stuff
}

void VAR(stop)() {
  // Don't check stack length, because it probably wont be empty.
  exit(EXIT_SUCCESS);
}

TABLE VAR(table)() {
  return NULL; // TODO
}

TABLE VAR(insert)(STRING key, cognate_object value, TABLE tab) {
  (void)key;
  (void)value;
  (void)tab;
  return NULL; // TODO
}

ANY VAR(get)(STRING key, TABLE tab) {
  (void)key;
  (void)tab; // TODO
  return OBJ(number,42);
}

LIST VAR(values)(TABLE tab) {
  (void)tab; // TODO
  return NULL;
}

BOOLEAN VAR(match)(STRING reg_str, STRING str) {
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

NUMBER VAR(ordinal)(STRING str) {
  if unlikely(str[0] && !str[1])
  {
    throw_error("Invalid string '%.64s' (should be length 1)", str);
  }
  wchar_t chr = 0;
  mbtowc(&chr, str, MB_CUR_MAX);
  return chr;
}

STRING VAR(character)(NUMBER d) {
  const wchar_t i = d;
  char* str = GC_MALLOC (MB_CUR_MAX + 1);
  if unlikely(i != d || wctomb(str, i) == -1) throw_error("Cannot convert %.14g to UTF8 character", d);
  str[mblen(str, MB_CUR_MAX)] = '\0';
  return str;
}

NUMBER VAR(floor)(NUMBER a) {
  return floor(a);
}

NUMBER VAR(round)(NUMBER a) {
  return round(a);
}

NUMBER VAR(ceiling)(NUMBER a) {
  return ceil(a);
}

void VAR(assert)(STRING name, BOOLEAN result) {
  if unlikely(!result)
  {
    throw_error("Assertion '%s' has failed", name);
  }
}

void VAR(error)(STRING str) {
  current_word_name = NULL;
  errno = 0;
  throw_error("%s", str);
}

LIST VAR(map)(BLOCK blk, LIST lst)
{
  if (lst)
  {
    push(lst->object);
    blk();
    cognate_list* node = GC_NEW(cognate_list);
    node->object = pop();
    node->next = VAR(map)(blk, lst->next);
    return node;
  }
  return NULL;
}

LIST VAR(filter)(BLOCK blk, LIST lst)
{
  if (lst)
  {
    push(lst->object);
    blk();
    if (CHECK(boolean, pop()))
    {
      cognate_list* node = GC_NEW(cognate_list);
      node->object = lst->object;
      node->next = VAR(filter)(blk, lst->next);
      return node;
    }
    return VAR(filter)(blk, lst->next);
  }
  return NULL;
}

void VAR(for)(LIST lst, BLOCK blk)
{
  if (lst)
  {
    push(lst->object);
    blk();
    VAR(for)(lst->next, blk);
  }
}

LIST VAR(range)(NUMBER start, NUMBER end, NUMBER step)
{
  if (start * step < end * step)
  {
    cognate_list* node = GC_NEW(cognate_list);
    node->object = OBJ(number, start);
    node->next = VAR(range)(start+step, end, step);
    return node;
  }
  return NULL;
}

static size_t mbstrlen(const char* str)
{
  // Get the number of characters in a multibyte string.
  // Normal strlen() gets number of bytes for some reason.
  size_t len = 0;
  for (; *str ; str += mblen(str, MB_CUR_MAX), ++len);
  return len;
}
