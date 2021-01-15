#ifndef FUNC_C
#define FUNC_C

#include "cognate.h"
#include "types.h"

static cognate_list params;

#define call(name) \
  word_name = #name; \
  cognate_function_ ## name();

// I'm not putting type signatures for every single function here.

#include "type.c"
#include "stack.c"
#include "table.c"
#include "io.c"

#include <gc/gc.h>
#include <unistd.h>
#include <regex.h>
#include <math.h>
#include <string.h>
#include <limits.h>

static void cognate_function_if(char* const if_status)
{
  // TODO: Else and ElseIf should only be allowed directly following an If.
  const cognate_block cond = pop(block);
  const cognate_block expr = pop(block);
  cond(); \
  if ((*if_status = pop(boolean)))
  {
    expr();
  }
}
#define cognate_function_if() cognate_function_if(&if_status)

static void cognate_function_else(char* const if_status)
{
  const cognate_block expr = pop(block);
  if (!*if_status)
  {
    expr();
  }
  else if unlikely(*if_status == 2)
  {
    throw_error("Else statement encountered before [Else]If statement!");
  }
  *if_status = 2;
}
#define cognate_function_else() cognate_function_else(&if_status)

static void cognate_function_elseif(char* const if_status)
{
  const cognate_block cond = pop(block);
  const cognate_block expr = pop(block);
  cond();
  if unlikely(*if_status == 2)
  {
    throw_error("ElseIf statement encountered before [Else]If statement!");
  }
  else if (pop(boolean) && !*if_status)
  {
    expr();
    *if_status = 1;
  }
}
#define cognate_function_elseif() cognate_function_elseif(&if_status)

static void cognate_function_while() {
  cognate_block cond = pop(block);
  cognate_block body = pop(block);
  cond();
  while (pop(boolean))
  {
    body();
    cond();
  }
}

static void cognate_function_do() { pop(block)(); }

static void cognate_function_put()   { print_object(pop_any(), 0); fflush(stdout); }
static void cognate_function_print() { print_object(pop_any(), 0); puts("");       }

static void cognate_function_sum()      { push(number, pop(number) + pop(number)); }
static void cognate_function_multiply() { push(number, pop(number) * pop(number)); }
static void cognate_function_divide()   { push(number, (1 / pop(number) * pop(number))); }
static void cognate_function_subtract() { push(number, (-pop(number) + pop(number))); }

static void cognate_function_modulo() {
  const double n = pop(number);
  push(number, fmod(pop(number), n));
}

static void cognate_function_random() {
  const double low  = pop(number);
  const double high = pop(number);
  const double step = pop(number);
  if unlikely(high < low)
  {
    throw_error("Cannot generate random number in range! (%.*g..%.*g)", DBL_DIG, low, high);
    return;
  }
  else if (high - low < step)
  {
    push(number, low);
    return;
  }
  // This is not cryptographically secure btw.
  // Since RAND_MAX may only be 2^15, we need to do this:
  const long num
    = ((long)(short)random())
    | ((long)(short)random() << 15)
    | ((long)(short)random() << 30)
    | ((long)(short)random() << 45)
    | ((long)       random() << 60);
  push(number, low + (double)fmod(num, (unsigned long)((high - low) / step)) * step);
}

static void cognate_function_drop()    { pop_any(); } // These can be defined within cognate.
static void cognate_function_twin()    { push_any(peek_any()); }
static void cognate_function_triplet() { const cognate_object a = peek_any(); push_any(a); push_any(a); }
static void cognate_function_swap()    { const cognate_object a = pop_any(); const cognate_object b = pop_any(); push_any(a); push_any(b); }
static void cognate_function_clear()   { stack.items.top=stack.items.start; }

static void cognate_function_true()  { push(boolean, 1); }
static void cognate_function_false() { push(boolean, 0); }

static void cognate_function_either() { push(boolean, pop(boolean) + pop(boolean)); } // Use unconventional operators to avoid short-circuits.
static void cognate_function_both()   { push(boolean, pop(boolean) & pop(boolean)); }
static void cognate_function_one_of() { push(boolean, pop(boolean) ^ pop(boolean)); }
static void cognate_function_not()    { push(boolean, !pop(boolean)); }


static void cognate_function_equal()          { push(boolean,  compare_objects(pop_any(),pop_any())); }
static void cognate_function_unequal()        { push(boolean, !compare_objects(pop_any(),pop_any())); }
static void cognate_function_preceed()        { push(boolean, pop(number) >  pop(number)); }
static void cognate_function_exceed()         { push(boolean, pop(number) <  pop(number)); }
static void cognate_function_equalorpreceed() { push(boolean, pop(number) >= pop(number)); }
static void cognate_function_equalorexceed()  { push(boolean, pop(number) <= pop(number)); }

static void cognate_function_number_()  { push(boolean, pop_any().type == number);  } // Question marks are converted to underscores.
static void cognate_function_list_()    { push(boolean, pop_any().type == list);    } // However all other symbols are too.
static void cognate_function_string_()  { push(boolean, pop_any().type == string);  } // So this is a temporary hack!
static void cognate_function_block_()   { push(boolean, pop_any().type == block);   }
static void cognate_function_boolean_() { push(boolean, pop_any().type == boolean); }

static void cognate_function_discard() {
  // O(n) where n is the number of element being Discarded.
  const double num = pop(number);
  const size_t num_discarding = num;
  if unlikely(num != num_discarding) throw_error("Number of elements to Discard must be positive integer, not %.*g!",DBL_DIG ,num);
  const cognate_list obj = *pop(list);
  if unlikely(num_discarding > (size_t)(obj.top - obj.start)) throw_error("List of length %zu is too small to Discard %zu elements from!", obj.top - obj.start, num_discarding);
  cognate_list* const lst = (cognate_list* const) cognate_malloc (sizeof(cognate_list));
  lst->start = obj.start + num_discarding;
  lst->top = obj.top;
  push(list, lst);
}

static void cognate_function_take() {
  // O(n) where n is the number of element being Taken.
  const double num = pop(number);
  const size_t num_taking = num;
  if unlikely(num != num_taking) throw_error("Number of elements to Take must be positive integer, not %.*g!", DBL_DIG, num);
  cognate_list obj = *pop(list);
  if unlikely(num_taking > (size_t)(obj.top - obj.start)) throw_error ("List of length %zu is too small to Take %zu elements from!", obj.top - obj.start, num_taking);
  cognate_list* const lst = (cognate_list*)cognate_malloc(sizeof(cognate_list));
  lst->start = obj.start;
  lst->top = lst->start + num_taking;
  push(list, lst);
}

static void cognate_function_index() {
  const double d = pop(number);
  const size_t index = d;
  if unlikely(index != d)
    throw_error("List Index must be a positive integer, not %15g!", d);
  const cognate_list lst = *pop(list);
  if unlikely(index >= (size_t)(lst.top - lst.start))
    throw_error("Index %zu is out of bounds! (list is of length %zu)", index, lst.top - lst.start);
  push_any(lst.start [index]);
}

static void cognate_function_length() {
  const cognate_list lst = *pop(list);
  push(number, (double)(lst.top - lst.start));
}

static void cognate_function_list() {
  // I solemnly swear that I will NEVER RETURN THE ADDRESS OF A LOCAL VARIABLE!
  // Get the block argument
  const cognate_block expr = pop(block);
  // Move the stack to temporary storage
  const cognate_stack temp_stack = stack;
  // Allocate a list as the stack
  init_stack();
  // Eval expr
  expr();
  // Store the resultant list, GC_realloc-ing to fit snugly in memory.
  cognate_list* const lst = (cognate_list*)cognate_malloc(sizeof(cognate_list));
  *lst = stack.items;
  // Restore the original stack
  stack = temp_stack;
  const long lst_len = lst->top - lst->start;
  lst->start = cognate_realloc(lst->start, lst_len * sizeof(cognate_object));
  lst->top = lst->start + lst_len;
  // Push the created list to the stack
  push(list, lst);
}

static void cognate_function_characters() {
  // Can be rewritten using substring, albeit O(n^2)
  const char* str = pop(string);
  size_t length = mbstrlen(str);
  cognate_list* const lst = (cognate_list*) cognate_malloc (sizeof(cognate_list));
  lst->top = lst->start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * length);
  lst->top += length;
  for (size_t i = 0; i < length; ++i)
  {
    size_t char_len = mblen(str, MB_CUR_MAX);
    char* const temp = (char*) cognate_malloc (char_len+1);
    memmove(temp, str, char_len);
    temp[char_len] = '\0';
    lst->start[i] = (cognate_object) {.type=string, .string=temp};
    str += char_len;
  }
  push(list, lst);
}

static void cognate_function_split() {
  // Can be rewritten using Substring.
  const char* const delimiter = pop(string);
  const size_t delim_size     = strlen(delimiter);
  if unlikely(!delim_size) throw_error("Cannot Split a string with a zero-length delimiter!");
  const char* str = pop(string);
  size_t length = 1;
    for (const char* temp = str; (temp = strstr(temp, delimiter) + delim_size) - delim_size; ++length);
  cognate_list* const lst = (cognate_list* const) cognate_malloc (sizeof(cognate_list));
  lst->top = lst->start   = (cognate_object*)     cognate_malloc (sizeof(cognate_object) * length);
  lst->top += length;
  size_t i = 0;
  for (const char* c; (c = strstr(str, delimiter)); i++)
  {
    char* const buf = (char* const) cognate_malloc (c - str + 1);
    strncpy(buf, str, c - str);
    buf[c - str] = '\0';
    lst->start[i] = (cognate_object) {.type=string, .string=buf};
    str = c + delim_size;
  }
  lst->start[i] = (cognate_object) {.type=string, .string=str};
  push(list, lst);
}

/*
static void cognate_function_split_regex() {
  const char* reg_str = pop(string);
  const char* str     = pop(string);
  regmatch_t pmatch[2];
  regex_t reg;
  regcomp(&reg, reg_str, REG_EXTENDED | REG_NEWLINE);
  cognate_list* lst = cognate_malloc (sizeof(cognate_list));
  lst->start = cognate_malloc (sizeof(cognate_object) * strlen(str)); // TODO
  lst->top = lst->start;
  while (regexec(&reg, str, 1, pmatch, 0) != REG_NOMATCH)
  {
    const size_t match_start = pmatch[0].rm_so;
    const size_t match_len = pmatch[0].rm_eo - match_start;
    char* buf = cognate_malloc(match_start + 1);
    strncpy(buf, str, match_start);
    buf[match_start] = '\0';
    *lst->top++ = (cognate_object) {.type=string, .string=buf};
    str += match_start + match_len;
  }
  *lst->top++ = (cognate_object) {.type=string, .string=str};
  push(list, lst);
}
*/

static void cognate_function_append() {
  // Joins a list to the end of another list.
  // Define Prepend (Swap, Append);
  const cognate_list lst1 = *pop(list);
  const cognate_list lst2 = *pop(list);
  const size_t list1_len = lst1.top - lst1.start;
  const size_t list2_len = lst2.top - lst2.start;
  const size_t new_lst_len = list1_len + list2_len;
  cognate_list* const new_lst = (cognate_list* const) cognate_malloc (sizeof(cognate_list));
  new_lst->top = new_lst->start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * new_lst_len);
  new_lst->top += new_lst_len;
  memmove(new_lst->start, lst2.start, list2_len * sizeof(cognate_object));
  memmove(new_lst->start+list2_len, lst1.start, list1_len * sizeof(cognate_object));
  push(list, new_lst);
}

static void cognate_function_suffix() {
  // Joins a string to the end of another string.
  // Define Prefix (Swap, Suffix);
  const char* const str1 = pop(string);
  const char* const str2 = pop(string);
  const size_t str1_size = strlen(str1);
  const size_t str2_size = strlen(str2);
  const size_t new_string_size = str1_size + str2_size;
  char* const new_str = (char* const) cognate_malloc (new_string_size);
  memmove(new_str, str2, str2_size);
  memmove(new_str+str2_size, str1, str1_size);
  push(string, new_str);
}

static void cognate_function_string_length() {
  push(number, mbstrlen(pop(string)));
}

static void cognate_function_substring() {
  // O(end).
  // Only allocates a new string if it has to.
  const double startf = pop(number);
  const double endf   = pop(number);
  size_t start  = startf;
  size_t end    = endf;
  if unlikely(start != startf || end != endf || start > end)
    throw_error("Cannot substring with character range %.*g...%.*g!", DBL_DIG, DBL_DIG, startf, endf);
  const char* str = pop(string);
  size_t str_size = 0;
  end -= start;
  for (;start != 0; --start)
  {
    str += mblen(str, MB_CUR_MAX);
  }
  for (;end != 0; --end)
  {
    if unlikely(str[str_size] == '\0') throw_error("String is too small (%li characters) to take substring from!", mbstrlen(str));
    str_size += mblen(str+str_size, MB_CUR_MAX);
  }
  if unlikely(str[str_size] == '\0')
  {
    // We don't need to make a new string here.
    push(string, str);
  }
  else
  {
    char* const sub_str = (char* const) cognate_malloc (str_size + 1);
    memmove(sub_str, str, str_size);
    sub_str[str_size] = '\0';
    push(string, sub_str);
  }
}

static void cognate_function_push() {
  cognate_list lst = *pop(list);
  for (cognate_object* obj = lst.start; obj != lst.top; ++obj)
  {
    // This can probably be optimised.
    push_any(*obj);
  }
}

static void cognate_function_input() {
  // Read user input to a string.
  size_t size = 0;
  char* buf;
  getline(&buf, &size, stdin);
  char* const text = (char* const) cognate_malloc (size); // Does size include room for '\0'?
  strcpy(text, buf);
  free(buf);
  push(string, text);
}

static void cognate_function_read() {
  // Read a file to a string.
  const char* const filename = pop(string);
  FILE *fp = fopen(filename, "r");
  if unlikely(fp == NULL) throw_error("Cannot open file '%s'. It probably doesn't exist.", filename);
  fseek(fp, 0L, SEEK_END);
  size_t file_size = ftell(fp);
  rewind(fp);
  char *text = (char*) cognate_malloc (file_size);
  fread(text, sizeof(char), file_size, fp);
  fclose(fp);
  text[file_size-1] = '\0'; // Remove trailing eof.
  push(string, text);
  // TODO: single line (or delimited) file read function for better IO performance?
}

static void cognate_function_number() {
  // casts string to number.
  const char* const str = pop(string);
  push(number, (double)strtof(str, NULL)); // strtof uses floats instead of doubles, prepare for rounding error.
}

static void cognate_function_path() {
  char buf[FILENAME_MAX];
  if (!getcwd(buf, FILENAME_MAX))
  {
    throw_error("Cannot get executable path!");
  }
  const size_t len = strlen(buf);
  char* const path = (char* const) cognate_malloc(len + 1);
  strcpy(path, buf);
  push(string, path);
}

static void cognate_function_stack() {
  copy_blocks();
  const size_t len = stack.items.top - stack.items.start;
  const size_t bytes = len * sizeof(cognate_object);
  cognate_list* lst = (cognate_list*) cognate_malloc (sizeof(cognate_list));
  lst->start = (cognate_object*) cognate_malloc (bytes);
  lst->top = lst->start + len;
  memmove(lst->start, stack.items.start, bytes);
  push(list, lst);
}

static void cognate_function_write() {
  // Write string to end of file, without a newline.
  FILE* const file = fopen(pop(string), "a");
  const char* const str = pop(string);
  fputs(str, file);
  fclose(file);
}

static void cognate_function_parameters() {
  push(list, &params);
}

static void cognate_function_stop() {
  // Don't check stack length, because it probably wont be empty.
  exit(0);
}

static void cognate_function_table() {
  cognate_function_list();
  const cognate_list init = *pop(list);
  const size_t table_size = ((init.top - init.start) * LIST_GROWTH_FACTOR);
  cognate_table* const tab = (cognate_table*) cognate_malloc (sizeof(cognate_table)); // Need to allocate list here.
  tab->items.start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * table_size);
  tab->items.top = tab->items.start + table_size;
  tab->confirmation_hash = (unsigned long*) cognate_malloc (sizeof(unsigned long) * table_size);
  const char *key;
  cognate_object value;
  for (const cognate_object *i = init.start + 1; i < init.top; i += 2)
  {
    key = check_type(string, *i).string;
    value = *(i-1);
    *tab = table_add(hash(key), value, *tab);
  }
  push(table, tab);
}

static void cognate_function_insert() {
  // O(n) :(
  const char* const key = pop(string);
  const cognate_object value = pop_any();
  cognate_table* const tab = (cognate_table*) cognate_malloc (sizeof(cognate_table));
  *tab = table_add(hash(key), value, table_copy(*pop(table)));
  push(table, tab);
}

static void cognate_function_get() {
  // O(1) mostly;
  const char* const key = pop(string);
  const cognate_table tab = *pop(table);
  push_any(table_get(key, tab));
}

static void cognate_function_values() {
  // O(n)
  // Resulting list is NOT in any order at all.
  // Equivilant tables may give differently ordered lists.
  const cognate_table tab = *pop(table);
  cognate_list* const lst = (cognate_list*) cognate_malloc (sizeof(cognate_list));
  const long table_size = tab.items.top - tab.items.start;
  lst->start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * table_size);
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
}

static void cognate_function_match() {
  // Returns true if string matches regex.
  static const char *old_str = NULL;
  const char* const reg_str = pop(string);
  static regex_t reg;
  if (old_str == NULL || strcmp(reg_str, old_str) != 0)
  {
    // Technically, the last regex to be used in the program will leak memory.
    // However, this is minor, since only a limited amount of memory can be leaked.
    regfree(&reg); // Apparently freeing an unallocated regex is fine.
    if unlikely(*reg_str == '\0' || regcomp(&reg, reg_str, REG_EXTENDED | REG_NEWLINE | REG_NOSUB))
    {
      throw_error("Cannot compile invalid regular expression! ('%s')", reg_str);
    }
    old_str = reg_str; /* This should probably be strcpy, but I trust that reg_str is either
                          allocated with the garbage collector, or read only in the data segment. */
  }
  const int found = regexec(&reg, pop(string), 0, NULL, 0);
  if unlikely(found != 0 && found != REG_NOMATCH)
  {
    throw_error("Regex match error! (%s)", reg_str);
    // If this error ever actually appears, use regerror to get the full text.
  }
  push(boolean, !found);
}

static void cognate_function_ordinal() {
  const char* const str = pop(string);
  if unlikely(mbstrlen(str) != 1)
  {
    throw_error("Ordinal requires string of length 1. String '%s' is not of length 1!", str);
  }
  int chr = '\0';
  mbtowc(&chr, str, MB_CUR_MAX);
  push(number, chr);
}

static void cognate_function_character() {
  const double d = pop(number);
  const int i = d;
  if unlikely(i != d) throw_error("Cannot convert %.*g to UTF8 character!", DBL_DIG, d);
  char* const str = (char* const) cognate_malloc (MB_CUR_MAX + 1);
  wctomb(str, i);
  push(string, str);
}

static void cognate_function_floor() {
  push(number, floor(pop(number)));
}

static void cognate_function_round() {
  push(number, round(pop(number)));
}

static void cognate_function_ceiling() {
  push(number, ceil(pop(number)));
}

static void cognate_function_assert() {
  const char* const name = pop(string);
  if unlikely(!pop(boolean))
  {
    throw_error("Assertion '%s' has failed!", name);
  }
}

static void cognate_function_error() {
  word_name = NULL;
  errno = 0;
  throw_error(pop(string));
}

#endif
