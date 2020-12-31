#ifndef FUNC_C
#define FUNC_C

#include "cognate.h"
#include "types.h"

static cognate_list params;

#ifdef debug
  #define call(name) debug_printf("Calling %s", #name); cognate_function_ ## name();
#else 
  #define call(name) cognate_function_ ## name();
#endif

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
  if ((*if_status = pop(boolean) && !*if_status))
  {
    expr();
  }
  else if unlikely(*if_status == 2)
  {
    throw_error("ElseIf statement encountered before [Else]If statement!");
  }
}
#define cognate_function_elseif() cognate_function_elseif(&if_status)

static void cognate_function_do() { pop(block)(); }

static void cognate_function_put()   { print_object(pop_any(), 1); fflush(stdout); }
static void cognate_function_print() { print_object(pop_any(), 1); puts("");       }

static void cognate_function_sum()      { push(number, pop(number) + pop(number)); }
static void cognate_function_multiply() { push(number, pop(number) * pop(number)); }
static void cognate_function_divide()   { push(number, (1 / pop(number) * pop(number))); }
static void cognate_function_subtract() { push(number, (-pop(number) + pop(number))); }

static void cognate_function_modulo() {
  const double n = pop(number);
  const double m = pop(number);
  if unlikely(m != (long long)m || n != (long long)n)
  {
    throw_error("Modulo should only take integer arguments. '%.15g modulo %.15g' is invalid.", n, m);
  }
  push(number, (long long)m % (long long)n);
}

static void cognate_function_random() {
  const double low  = pop(number);
  const double high = pop(number);
  const double step = pop(number);
  if unlikely(high < low)
  {
    throw_error("Cannot generate random number in range! (%.15g..%.15g)", low, high);
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
  push(number, low + (double)(num % (unsigned long)((high - low + step) / step)) * step); 
}

static void cognate_function_drop()    { pop_any(); } // These can be defined within cognate.
static void cognate_function_twin()    { push_any(peek_any()); }
static void cognate_function_triplet() { const cognate_object a = peek_any(); push_any(a); push_any(a); }
static void cognate_function_swap()    { const cognate_object a = pop_any(); const cognate_object b = pop_any(); push_any(a); push_any(b); }
static void cognate_function_clear()   { init_stack(); }

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
  if unlikely(num != num_discarding) throw_error("Number of elements to Discard must be positive integer, not %.15g!", num);
  const cognate_list obj = *pop(list);
  cognate_list* const lst = (cognate_list* const) cognate_malloc (sizeof(cognate_list));
  *lst = obj;
  if unlikely(num_discarding > list_len(*lst)) throw_error("List of length %zu is too small to Discard %zu elements from!", list_len(*lst), num_discarding);
  lst->start += num_discarding;
  push(list, lst);
}

static void cognate_function_take() {
  // O(n) where n is the number of element being Taken.
  const double num = pop(number);
  const size_t num_taking = num;
  if unlikely(num != num_taking) throw_error("Number of elements to Take must be positive integer, not %.15g!", num);
  cognate_list obj = *pop(list);
  cognate_list* const lst = (cognate_list*)cognate_malloc(sizeof(cognate_list));
  *lst = obj;
  if unlikely(num_taking > list_len(*lst)) throw_error ("List of length %zu is too small to Take %zu elements from!", list_len(*lst), num_taking);
  lst->top = lst->start + num_taking;
  push(list, lst);
}

static void cognate_function_index() { 
  const double d = pop(number);
  const size_t index = d;
  if unlikely(index != d)
    throw_error("List Index must be a positive integer, not %15g!", d);
  const cognate_list lst = *pop(list);
  if unlikely(index >= list_len(lst))
    throw_error("Index %zu is out of bounds! (list is of length %zu)", index, list_len(lst));
  push_any(lst.start [index]);
}

static void cognate_function_length() {
  const cognate_list lst = *pop(list);
  push(number, (double)list_len(lst));
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
  const long lst_len = list_len(*lst);
  lst->start = cognate_realloc(lst->start, lst_len * sizeof(cognate_object));
  lst->top = lst->start + lst_len;
  // Push the created list to the stack
  push(list, lst);
}

static void cognate_function_characters() {
  const char* const str = pop(string);
  cognate_list* const lst = (cognate_list*)cognate_malloc(sizeof(cognate_list));
  size_t length = strlen(str);
  lst->start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * length);
  lst->top = lst->start + length;
  while (length --> 0)
  {
    char* const temp = (char*) cognate_malloc (2);
    temp[0] = str[length];
    temp[1] = '\0';
    lst->start[length] = ((cognate_object){.type=string, .string=temp});
  }
  push(list, lst);
}

static void cognate_function_join() {
  // Joins a list of strings into a single string.
  const char* const delimiter = pop(string);
  const long delim_size = strlen(delimiter);
  const cognate_list lst = *pop(list);
  long str_size = 1; // Add extra 1 for \0
  const cognate_object *i;
  for (i = lst.start; i < lst.top; ++i)
  {
    str_size += strlen(check_type(string, *i).string) + delim_size;
  }
  char* const str = (char*) cognate_malloc (str_size);
  strcpy(str, lst.start->string);
  for (i = lst.start+1; i < lst.top; ++i)
  {
    strcat(str, delimiter);
    strcat(str, i->string);
  }
  push(string, str);
}

static void cognate_function_stack() {
  push(list, &stack.items);
}

static void cognate_function_append() {
  const cognate_list lst1 = *pop(list);
  const cognate_list lst2 = *pop(list);
  const size_t list1_len = list_len(lst1);
  const size_t list2_len = list_len(lst2);
  const size_t new_list_len = list1_len + list2_len;
  cognate_list* const lst = (cognate_list* const) cognate_malloc (sizeof(cognate_list));
  lst->start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * new_list_len);
  lst->top = lst->start + new_list_len;
  memmove(lst->start, lst2.start, list2_len * sizeof(cognate_object));
  memmove(lst->start+list2_len, lst1.start, list1_len * sizeof(cognate_object));
  push(list, lst);
}

static void cognate_function_input() {
  // Read user input to a string.
  size_t size = 0;
  char* buf;
  getline(&buf, &size, stdin);
  char* const text = (char* const) cognate_malloc (size);
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
  // get_current_dir_name() allocates memory, so we must copy it to a GC'd buffer.
  char* buf = get_current_dir_name();
  const size_t size = strlen(buf) + 1;
  char* cwd = (char*) cognate_malloc (size);
  strcpy(cwd, buf);
  free(buf);
  push(string, cwd);
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
  call(list);
  const cognate_list init = *pop(list);
  const size_t table_size = (list_len(init) * LIST_GROWTH_FACTOR);
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
  const long table_size = list_len(tab.items);
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
    if unlikely(*reg_str == '\0' || regcomp(&reg, reg_str, REG_EXTENDED | REG_NEWLINE))
    {
      throw_error("Cannot compile invalid regular expression! ('%s')", reg_str); 
    }
    if unlikely(regcomp(&reg, reg_str, REG_EXTENDED|REG_NEWLINE))
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
  if unlikely(str[0] == '\0' || str[1] != '\0')
  {
    throw_error("Ordinal requires string of length 1. String '%s' is not of length 1!", str);
  }
  push(number, str[0]);
}

static void cognate_function_character() {
  const double d = pop(number);
  const long i = d;
  if unlikely(i != d)           throw_error("Cannot convert non-integer (%.15g) to ASCII character!", d);
  if unlikely(i < 0 || i > 255) throw_error("Value (%li) is not in ASCII character range!", i);
  char* const str = (char* const) cognate_malloc (2);
  str[0] = i;
  str[1] = '\0';
  push(string, str);
}

static void cognate_function_parsenumber() {
  const char* const str = pop(string);
  const double num;
  push(number, atof(str));
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
  throw_error(pop(string));
}

#endif
