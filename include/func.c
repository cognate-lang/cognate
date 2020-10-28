#ifndef FUNC_C
#define FUNC_C

#include "stack.c"
#include "type.c"
#include "io.c"
#include <Block.h>
#include <unistd.h>
#include <limits.h>

#define INITIAL_INPUT_SIZE 64
#define PATH_MAX 4096

// Macro to define external cognate function.
#define external_function(name, body) \
  static void cognate_function_ ## name () body

// Macro for defining external cognate variables with specified type.
#define external_variable(name, type, value) \
  static void cognate_function_ ## name () { \
    push(type, value); }

#ifdef debug
  #define call(name) {fprintf(stderr, "[DEBUG] %s:%d -> Calling %s\n", __FILE__, __LINE__, #name); cognate_function_ ## name();}
#else 
  #define call(name) {cognate_function_ ## name();}
#endif

cognate_list params;

external_function(do,             { pop(block)();                         })
external_function(print,          { print_object(pop_any(), 1); puts(""); })

external_function(sum,            { push(number, pop(number) + pop(number));                           })
external_function(product,        { push(number, pop(number) * pop(number));                           })
external_function(divisor,        { push(number, (1 / pop(number) * pop(number)));                     })
external_function(difference,     { push(number, (-pop(number) + pop(number)));                        })
external_function(modulo,         { int n = pop(number); push(number, (double)((int)pop(number) % n)); }) // TODO: add checking if integer.
/*
external_function(random,         { double low = pop(number); double high = pop(number); double step = pop(number); 
                                    if (high < low) throw_error("Cannot generate random number in range!");
                                    else if (high - low < step) push(number, low);
                                    else push(number, low + (double)(rand() % (int)((high - low) / step)) * step); })
*/
external_function(drop,           { pop_any();                                                                            })
external_function(twin,           { push_any(peek_any());                                                                 })
external_function(triplet,        { cognate_object a = peek_any(); push_any(a); push_any(a);                              })
external_function(swap,           { cognate_object a = pop_any(); cognate_object b = pop_any(); push_any(a); push_any(b); })
external_function(clear,          { init_stack();                                                                         })

external_variable(true,  boolean, 1)
external_variable(false, boolean, 0)

external_function(either, { _Bool a = pop(boolean); push(boolean, pop(boolean) || a);}) // Beware short circuiting.
external_function(both,   { _Bool a = pop(boolean); push(boolean, pop(boolean) && a );})
external_function(one_of, { _Bool a = pop(boolean); _Bool b = pop(boolean); push(boolean, (a && !b) || (!a && b)); })
external_function(not,    { push(boolean, !pop(boolean)); })


external_function(equal,          { push(boolean,  compare_objects(pop_any(),pop_any())); })
external_function(unequal,        { push(boolean, !compare_objects(pop_any(),pop_any())); })
external_function(preceed,        { push(boolean, pop(number) >  pop(number)); })
external_function(exceed,         { push(boolean, pop(number) <  pop(number)); })
external_function(equalorpreceed, { push(boolean, pop(number) >= pop(number)); })
external_function(equalorexceed,  { push(boolean, pop(number) <= pop(number)); })

external_function(number_,  {push(boolean, pop_any().type == number);}) // Question marks are converted to underscores.
external_function(list_,    {push(boolean, pop_any().type == list);})   // However all other synbols are too.
external_function(string_,  {push(boolean, pop_any().type == string);}) // So this is a temporary hack!
external_function(block_,   {push(boolean, pop_any().type == block);})
external_function(boolean_, {push(boolean, pop_any().type == boolean);})

external_function(discard, { 
  // O(n) where n is the number of element being Discarded.
  double num = (int)pop(number);
  int num_discarding = num;
  if (num != num_discarding) throw_error("Cannot Discard a non-integer number of elements!");
  if (num_discarding < 0) throw_error("Cannot Discard a negative number of elements!");
  cognate_list obj = *pop(list);
  cognate_list *lst = (cognate_list*)malloc(sizeof(cognate_list));
  *lst = obj;
  if ((lst->start += num_discarding) > lst->top) throw_error("List is too small to Discard from!");
  push(list, lst);
})

external_function(take, {
  // O(n) where n is the number of element being Taken.
  double num = pop(number);
  int num_taking = num;
  if (num != num_taking) throw_error("Cannot Take a non-integer number of elements!");
  if (num_taking < 0) throw_error("Cannot Take a negative number of elements!");
  cognate_list obj = *pop(list);
  cognate_list *lst = (cognate_list*)malloc(sizeof(cognate_list));
  *lst = obj;
  if (lst->start + num_taking > lst->top) throw_error("List is too small to Take from!");
  lst->top = lst->start + num_taking;
  push(list, lst);
})

external_function(index, { 
  int index = pop(number);
  cognate_list lst = *pop(list);
  if (lst.start + index >= lst.top)
    throw_error("List index out of bounds!");
  push_any(lst.start [index]);
})

external_function(length,{
  cognate_list lst = *pop(list);
  push(number, (double)(lst.top - lst.start));
})

external_function(list,  { 
  // I solemnly swear that I will NEVER RETURN THE ADDRESS OF A LOCAL VARIABLE!
  // Get the block argument
  cognate_block expr = pop(block);
  // Move the stack to temporary storage
  cognate_stack temp_stack = stack;
  // Allocate a list as the stack
  init_stack();
  // Eval expr
  expr();
  // Store the resultant list, GC_REALLOC-ing to fit snugly in memory.
  cognate_list* lst = (cognate_list*)malloc(sizeof(cognate_list));
  *lst = stack.items;
  //TODO: Shrink the list to fit here with GC_REALLOC(). Previous attempts caused problems with -O0
  //lst->start = GC_REALLOC(lst->start, (lst->top - lst->start) * sizeof(cognate_object));
  //lst->top = lst->start + (int)stack.items.top - stack.items.start;
  // Restore the original stack
  stack = temp_stack;
  // Push the created list to the stack
  push(list, lst);
})

external_function(characters, {
  char* str = pop(string);
  cognate_list* lst = (cognate_list*)malloc(sizeof(cognate_list));
  lst->start = (cognate_object*) malloc (sizeof(cognate_object) * strlen(str));
  lst->top = lst->start + strlen(str);
  for (int i = strlen(str); i >= 0; --i)
  {
    // This segfaults with GC_MALLOC, but GC_MALLOC_ATOMIC seems to work.
    // TODO: find out why this even works.
    char *temp = (char*) malloc_atomic (sizeof(char) * 2);
    temp[0] = str[i];
    temp[1] = '\0';
    lst->start[i] = ((cognate_object){.type=string, .string=temp});
  }
  push(list, lst);
})

external_function(join, {
  // Joins list of single characters.
  // TODO: join list of full strings.
  cognate_list lst = *pop(list);
  const size_t str_size = lst.top - lst.start + 1;
  char* str = (char*) malloc (sizeof(char) * (str_size));
  str[str_size - 1] = '\0';
  for (int i = 0; i < lst.top - lst.start; ++i)
  {
    str[i] = check_type(string, lst.start[i]).string[0];
  }
  push(string, str);
})

external_function(stack,
{
  push(list, &stack.items);
})

external_function(if,
{
  cognate_block cond    = pop(block);
  cognate_block ifTrue  = pop(block);
  cognate_block ifFalse = pop(block);
  cond();
  pop(boolean) ? ifTrue() : ifFalse();
})

external_function(append,
{
  cognate_list lst1 = *pop(list);
  cognate_list lst2 = *pop(list);
  int list1_len = lst1.top - lst1.start;
  int list2_len = lst2.top - lst2.start;
  int new_list_len = list1_len + list2_len;
  cognate_list *lst = (cognate_list*) malloc (sizeof(cognate_list));
  lst->start = (cognate_object*) malloc (sizeof(cognate_object) * new_list_len);
  lst->top = lst->start + new_list_len;
  memcpy(lst->start, lst2.start, list2_len * sizeof(cognate_object));
  memcpy(lst->start+list2_len, lst1.start, list1_len * sizeof(cognate_object));
  push(list, lst);
})

external_function(input, {
  size_t str_size = INITIAL_INPUT_SIZE;
  char* str = (char*) malloc (str_size * sizeof(char));
  char* temp = str;
  char c;
  while((c = getchar()) != '\n' && c != EOF)
  {
    *str++ = c; 

    if (temp + str_size == str)
    {
      temp = realloc (temp, (str_size << 1));
      str = temp + str_size;
    }
  }
  *str = '\0';
  push(string, temp);
})

external_function(read, {
  // read a file.
  // TODO: use path of executable instead of user's path
  char* file_name  = pop(string);
  FILE *fp = fopen(file_name, "r");
  if (fp == NULL) throw_error("Cannot open file! It probably doesn't exist.");
  fseek(fp, 0L, SEEK_END);
  size_t file_size = ftell(fp);
  fseek(fp, 0L, SEEK_SET);
  char* file_data = (char*) malloc (file_size * sizeof(char));
  fread(file_data, sizeof(char), file_size, fp);
  fclose(fp);
  file_data[file_size-1] = '\0'; // Remove trailing newline (for easier splitting, printing, etc).
  push(string, file_data);
  //TODO: single line (or delimited) file reading for better IO performance?
})

external_function(number, {
  // casts string to number. 
  char* str = pop(string);
  push(number, (double)strtof(str, NULL)); // strtof uses floats instead of doubles, prepare for rounding error.
})

external_function(path, {
  // Get working directory path.
  char cwd[PATH_MAX]; // Much too big.
  if (getcwd(cwd, sizeof(cwd)) == NULL)
    throw_error("Cannot get current directory!");
  char *small_cwd = (char*) malloc (sizeof(char) * strlen(cwd)); // Much better size.
  strcpy(small_cwd, cwd);
  push(string, small_cwd);
})

external_function(write, {
  // Write string to end of file, without a newline.
  // TODO: use path of program instead of user's path.
  FILE *file = fopen(pop(string), "a"); 
  char *str = pop(string);
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
  // TODO
  call(list);
  cognate_list init = *pop(list);
  unsigned long table_size = init.top - init.start;
  cognate_table *tab = (cognate_table*) malloc (sizeof(cognate_table)); // Need to allocate list here.
  tab->items.start = (cognate_object*) calloc (table_size, sizeof(cognate_object) * table_size);
  tab->items.top = tab->items.start + table_size;
  tab->confirmation_hash = (long unsigned int*) malloc (sizeof(long unsigned int) * table_size);
  char *key;
  cognate_object value;
  for (cognate_object *i = init.start; i < init.top - 1; i += 2)
  {
    key = check_type(string, *(i+1)).string;
    value = *i;
    table_add(key, value, tab);
  }
  push(table, tab);
})

external_function(insert, {
  char *key = pop(string);
  cognate_object value = pop_any();
  cognate_table *tab = pop(table);
  table_add(key, value, tab);
})

external_function(get, {
  char *key = pop(string);
  cognate_table tab = *pop(table);
  push_any(table_get(key, tab));
})

#endif
