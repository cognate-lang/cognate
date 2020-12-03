#ifndef FUNC_C
#define FUNC_C

#include "cognate.h"
#include "stack.c"
#include "table.c"
#include "io.c"
#include <unistd.h>
#include <regex.h>
#include <math.h>
//#include <openssl/rand.h> TODO


static cognate_list params;

static _Bool if_status = 0;

external_function(do,             { pop(block)();                               })
external_function(put,            { print_object(pop_any(), 1); fflush(stdout); })
external_function(print,          { print_object(pop_any(), 1); puts("");       })

external_function(sum,            { push(number, pop(number) + pop(number)); })
external_function(product,        { push(number, pop(number) * pop(number)); })
external_function(divisor,        { push(number, (1 / pop(number) * pop(number))); })
external_function(difference,     { push(number, (-pop(number) + pop(number))); })

external_function(modulo, { 
  const double n = pop(number);
  const double m = pop(number);
  if (unlikely(m != (long long)m || n != (long long)n))
  {
    throw_error_fmt("Modulo should only take integer arguments. '%.15g modulo %.15g' is invalid.", n, m);
  }
  push(number, (long long)m % (long long)n);
})

external_function(random, { // This function is pretty broken.
  double low = pop(number); double high = pop(number); double step = pop(number); 
  if (unlikely(high < low))   { throw_error_fmt("Cannot generate random number in range! (%.15g..%.15g)", low, high); }
  else if (high - low < step) { push(number, low); }
  else { push(number, low + (double)(rand() % (size_t)((high - low + step) / step)) * step); }
})

external_function(drop,           { pop_any(); }) // These can be defined within cognate.
external_function(twin,           { push_any(peek_any()); })
external_function(triplet,        { const cognate_object a = peek_any(); push_any(a); push_any(a); })
external_function(swap,           { const cognate_object a = pop_any(); const cognate_object b = pop_any(); push_any(a); push_any(b); })
external_function(clear,          { init_stack(); })

external_variable(true,  boolean, 1)
external_variable(false, boolean, 0)

external_function(either, { const _Bool a = pop(boolean); push(boolean, pop(boolean) || a); }) // Beware short circuiting.
external_function(both,   { const _Bool a = pop(boolean); push(boolean, pop(boolean) && a); })
external_function(one_of, { const _Bool a = pop(boolean); const _Bool b = pop(boolean); push(boolean, (a && !b) || (!a && b)); })
external_function(not,    { push(boolean, !pop(boolean)); })


external_function(equal,          { push(boolean,  compare_objects(pop_any(),pop_any())); })
external_function(unequal,        { push(boolean, !compare_objects(pop_any(),pop_any())); })
external_function(preceed,        { push(boolean, pop(number) >  pop(number)); })
external_function(exceed,         { push(boolean, pop(number) <  pop(number)); })
external_function(equalorpreceed, { push(boolean, pop(number) >= pop(number)); })
external_function(equalorexceed,  { push(boolean, pop(number) <= pop(number)); })

external_function(number_,  { push(boolean, pop_any().type == number);  }) // Question marks are converted to underscores.
external_function(list_,    { push(boolean, pop_any().type == list);    })   // However all other synbols are too.
external_function(string_,  { push(boolean, pop_any().type == string);  }) // So this is a temporary hack!
external_function(block_,   { push(boolean, pop_any().type == block);   })
external_function(boolean_, { push(boolean, pop_any().type == boolean); })

external_function(discard, { 
  // O(n) where n is the number of element being Discarded.
  const double num = pop(number);
  const size_t num_discarding = num;
  if (unlikely(num != num_discarding)) throw_error_fmt("Cannot Discard a non-integer number of elements! (%.15g)", num);
  if (unlikely(num_discarding < 0)) throw_error_fmt("Cannot Discard a negative number of elements! (%zi)", num_discarding);
  const cognate_list obj = *pop(list);
  cognate_list* const lst = (cognate_list* const) cognate_malloc (sizeof(cognate_list));
  *lst = obj;
  if (unlikely((lst->start += num_discarding) > lst->top)) throw_error("List is too small to Discard from!");
  push(list, lst);
})

external_function(take, {
  // O(n) where n is the number of element being Taken.
  const double num = pop(number);
  const size_t num_taking = num;
  if (unlikely(num != num_taking)) throw_error_fmt("Cannot Take a non-integer number of elements! (%.15g)", num);
  if (unlikely(num_taking < 0)) throw_error_fmt("Cannot Take a negative number of elements! (%zi)", num_taking);
  cognate_list obj = *pop(list);
  cognate_list* const lst = (cognate_list*)cognate_malloc(sizeof(cognate_list));
  *lst = obj;
  if (unlikely(lst->start + num_taking > lst->top)) throw_error_fmt ("List is too small to Take from! (length %zu)", lst->top - lst->start);
  lst->top = lst->start + num_taking;
  push(list, lst);
})

external_function(index, { 
  const double d = pop(number);
  const size_t index = d;
  if (unlikely(index != (size_t)d))
    throw_error_fmt("List index (%.15g) should be an integer!", d);
  if (unlikely(index < 0))
    throw_error_fmt("Cannot have negative list index! (%zi)", index);
  const cognate_list lst = *pop(list);
  if (unlikely(lst.start + index >= lst.top))
    throw_error_fmt("Index %zi is out of bounds!", index);
  push_any(lst.start [index]);
})

external_function(length,{
  const cognate_list lst = *pop(list);
  push(number, (double)(lst.top - lst.start));
})

external_function(list,  { 
  // I solemnly swear that I will NEVER RETURN THE ADDRESS OF A LOCAL VARIABLE!
  // Get the block argument
  const cognate_block expr = pop(block);
  // Move the stack to temporary storage
  const cognate_stack temp_stack = stack;
  // Allocate a list as the stack
  init_stack();
  // Eval expr
  expr();
  // Store the resultant list, GC_cognate_realloc-ing to fit snugly in memory.
  cognate_list* lst = (cognate_list*)cognate_malloc(sizeof(cognate_list));
  *lst = stack.items;
  // Restore the original stack
  stack = temp_stack;
  const long lst_len = lst->top - lst->start;
  lst->start = cognate_realloc(lst->start, lst_len * sizeof(cognate_object));
  lst->top = lst->start + lst_len;
  // Push the created list to the stack
  push(list, lst);
})

external_function(characters, {
  const char* const str = pop(string);
  cognate_list* const lst = (cognate_list*)cognate_malloc(sizeof(cognate_list));
  const long length = strlen(str);
  lst->start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * length);
  lst->top = lst->start + length;
  for (int i = length; i >= 0; --i)
  {
    // This segfaults with malloc, but malloc_atomic seems to work.
    char* const temp = (char*) cognate_malloc_atomic (sizeof(char) * 2);
    temp[0] = str[i];
    temp[1] = '\0';
    lst->start[i] = ((cognate_object){.type=string, .string=temp});
  }
  push(list, lst);
})

external_function(join, {
  // Joins a list of strings into a single string.
  const cognate_list lst = *pop(list);
  long str_size = 0;
  const cognate_object *i;
  for (i = lst.start; i < lst.top; ++i)
  {
    str_size += strlen(check_type(string, *i).string);
  }
  char* const str = (char*) cognate_malloc (sizeof(char) * (str_size));
  for (i = lst.start; i < lst.top; ++i)
  {
    strcat(str, i->string);
  }
  push(string, str);
})

external_function(stack,
{
  push(list, &stack.items);
})

external_function(if,
{
  // TODO: Else and ElseIf should only be allowed directly following an If.
  const cognate_block cond = pop(block);
  const cognate_block expr = pop(block);
  cond();
  if (pop(boolean))
  {
    expr();
    if_status = 0;
    return;
  }
  if_status = 1;
})
  
external_function(else,
{
  const cognate_block expr = pop(block);
  if (if_status)
  {
    expr(); 
  }
})

external_function(elseif,
{
  const cognate_block cond = pop(block);
  const cognate_block expr = pop(block);
  cond();
  if (pop(boolean) && if_status)
  {
    expr();
    if_status = 0;
    return;
  }
  if_status = 1;
})

external_function(append,
{
  const cognate_list lst1 = *pop(list);
  const cognate_list lst2 = *pop(list);
  const size_t list1_len = lst1.top - lst1.start;
  const size_t list2_len = lst2.top - lst2.start;
  const size_t new_list_len = list1_len + list2_len;
  cognate_list* const lst = (cognate_list* const) cognate_malloc (sizeof(cognate_list));
  lst->start = (cognate_object*) cognate_malloc (sizeof(cognate_object) * new_list_len);
  lst->top = lst->start + new_list_len;
  memcpy(lst->start, lst2.start, list2_len * sizeof(cognate_object));
  memcpy(lst->start+list2_len, lst1.start, list1_len * sizeof(cognate_object));
  push(list, lst);
})

external_function(input, {
  // Read user input to a string.
  char *text = (char*) cognate_malloc (sizeof(char) * INITIAL_LIST_SIZE);
  size_t i = 0;
  size_t file_size = INITIAL_LIST_SIZE;
  while ((text[i++] = fgetc(stdin)) != '\n')
  {
    (i >= file_size) && (text = (char*) cognate_realloc (text, (file_size *= LIST_GROWTH_FACTOR)));
  }
  text[i-1] = '\0';
  push(string, text);
})

external_function(read, {
  // Read a file to a string.
  strcpy(file_name_buf, exe_dir);
  strcat(file_name_buf, "/");
  strcat(file_name_buf, pop(string));
  FILE *fp = fopen(file_name_buf, "r");
  if (unlikely(fp == NULL)) throw_error_fmt("Cannot open file '%s'. It probably doesn't exist.", file_name_buf);
  fseek(fp, 0L, SEEK_END);
  size_t file_size = ftell(fp);
  rewind(fp);
  char *text = (char*) cognate_malloc (sizeof(char) * file_size);
  fread(text, sizeof(char), file_size, fp);
  fclose(fp);
  text[file_size-1] = '\0'; // Remove trailing eof.
  push(string, text);
  // TODO: single line (or delimited) file read function for better IO performance?
})

external_function(number, {
  // casts string to number. 
  const char* const str = pop(string);
  push(number, (double)strtof(str, NULL)); // strtof uses floats instead of doubles, prepare for rounding error.
})

external_function(path, {
  // Get working directory path, TODO function to get executable path.
  if (getcwd(file_name_buf, PATH_MAX) == NULL)
    throw_error("Cannot get current directory!");
  char* small_cwd = strdup(file_name_buf);
  push(string, small_cwd);
})

external_function(write, {
  // Write string to end of file, without a newline.
  strcat(file_name_buf, exe_dir);
  strcat(file_name_buf, "/");
  strcat(file_name_buf, pop(string));
  FILE* const file = fopen(file_name_buf, "a"); 
  const char* const str = pop(string);
  fprintf(file, "%s", str);
  fclose(file);
})

external_function(parameters, {
  push(list, &params);
})

external_function(stop, {
  // Don't check stack length, because it probably wont be empty.
  exit(0);
})

external_function(table, {
  call(list);
  const cognate_list init = *pop(list);
  // The 2 on this line should probably be tuned or something.
  const unsigned long table_size = ((init.top - init.start) * 2) + MIN_TABLE_SIZE;
  cognate_table* const tab = (cognate_table*) cognate_malloc (sizeof(cognate_table)); // Need to allocate list here.
  tab->items.start = (cognate_object*) calloc (table_size, sizeof(cognate_object) * table_size);
  tab->items.top = tab->items.start + table_size;
  tab->confirmation_hash = (long unsigned int*) cognate_malloc (sizeof(long unsigned int) * table_size);
  const char *key;
  cognate_object value;
  for (const cognate_object *i = init.start; i < init.top - 1; i += 2)
  {
    key = check_type(string, *(i+1)).string;
    value = *i;
    *tab = table_add(hash(key), value, *tab);
  }
  push(table, tab);
})

external_function(insert, {
  // O(n) :(
  const char* const key = pop(string);
  const cognate_object value = pop_any();
  cognate_table* const tab = (cognate_table*) cognate_malloc (sizeof(cognate_table));
  *tab = table_add(hash(key), value, table_copy(*pop(table)));
  push(table, tab);
})

external_function(get, {
  // O(1) mostly;
  const char* const key = pop(string);
  const cognate_table tab = *pop(table);
  push_any(table_get(key, tab));
})

external_function(values, {
  // O(n)
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
})

external_function(match, {
  // Returns true if string matches regex.
  static const char *old_str = ""; // Can confirm_hash() return -1, if so this could be problematic.
  const char* const reg_str = pop(string);
  static regex_t reg;
  if (strcmp(reg_str, old_str) != 0)
  {
    regfree(&reg); // Apparently freeing an unallocated regex is fine.
    if (unlikely(regcomp(&reg, reg_str, REG_EXTENDED|REG_NEWLINE)))
    {
      throw_error_fmt("Cannot compile invalid regular expression! (%s)", reg_str); 
    }
    old_str = reg_str; /* This should probably be strcpy, but I trust that reg_str is either
                          allocated with the garbage collector, or read only in the data segment. */
  }
  const int found = regexec(&reg, pop(string), 0, NULL, 0);
  if (unlikely(found != 0 && found != REG_NOMATCH))
  {
    throw_error_fmt("Regex match error! (%s)", reg_str);
    // If this error ever actually appears, use regerror to get the full text.
  }
  push(boolean, !found);
})

external_function(ordinal, {
  const char* const str = pop(string);
  if (unlikely(strlen(str) != 1))
  {
    throw_error_fmt("Ordinal requires string of length 1. String '%s' is not of length 1!", str);
  }
  push(number, str[0]);
})

external_function(character, {
  const double d = pop(number);
  const long i = d;
  if (unlikely(i != d))           throw_error_fmt("Cannot convert non-integer (%.15g) to ASCII character!", d);
  if (unlikely(i < 0 || i > 255)) throw_error_fmt("Value (%li) is not in ASCII character range!", i);
  char* const str = (char* const) cognate_malloc (sizeof(char) * 2);
  str[0] = i;
  str[1] = '\0';
  push(string, str);
})

external_function(parsenumber,
{
  const char* const str = pop(string);
  const double num;
  push(number, atof(str));
})

external_function(floor,
{
  push(number, floor(pop(number)));
})

external_function(round,
{
  push(number, round(pop(number)));
})

external_function(ceiling,
{
  push(number, ceil(pop(number)));
})

external_function(assert,
{
  const char* const name = pop(string);
  const _Bool cond = pop(boolean);
  if (unlikely(!cond))
  {
    throw_error_fmt("Assertion '%s' has failed!", name);
  }
})

#endif
