#ifndef FUNC_C
#define FUNC_C

#include "stack.c"
#include "type.c"
#include "io.c"
#include <Block.h>

// Macro to define external cognate function.
#define external_function(name, body) \
  void cognate_function_ ## name () body

// Macro for defining external cognate variables with specified type.
#define external_variable(name, type, value) \
  void cognate_function_ ## name () { \
    push(type, value); }

#ifdef EBUG
  #define call(name) {fprintf(stderr, "[DEBUG] %s:%d -> Calling %s\n", __FILE__, __LINE__, #name); /*check_recursion_depth();*/ cognate_function_ ## name();}
#else 
  #define call(name) {/*check_recursion_depth();*/ cognate_function_ ## name();}
#endif

#define malloc GC_MALLOC
#define realloc GC_REALLOC

external_function(do,             { pop(block)();                             })
external_function(print,          { print_object(pop_any(), 1); puts("");  })

external_function(sum,            { push(number, pop(number) + pop(number));               })
external_function(product,        { push(number, pop(number) * pop(number));               })
external_function(divisor,        { double n = pop(number); push(number, pop(number) / n); })
external_function(difference,     { double n = pop(number); push(number, pop(number) - n); })
external_function(modulo,         { int n = pop(number); push(number, (double)((int)pop(number) % n)); }) // TODO: add checking if integer.

external_function(random,         { double low = pop(number); double high = pop(number); double step = pop(number); 
                               push(number, low + (double)(rand() % (int)((high - low) / step)) * step); })

external_function(drop,           { pop_any();                                                                                     })
external_function(twin,           { push_any(peek_any());                                                                       })
external_function(triplet,        { cognate_object a = peek_any(); push_any(a); push_any(a);                                    })
external_function(swap,           { cognate_object a = pop_any(); cognate_object b = pop_any(); push_any(a); push_any(b);          })
external_function(clear,          { init_stack();                                                                                  })

external_variable(true,  block, ^{ push(boolean, 1); })
external_variable(false, block, ^{ push(boolean, 0); })


external_function(equal,          { push(boolean, pop(number) == pop(number)); })
external_function(notequal,       { push(boolean, pop(number) != pop(number)); })
external_function(preceed,        { push(boolean, pop(number) >  pop(number)); })
external_function(exceed,         { push(boolean, pop(number) <  pop(number)); })
external_function(equalorpreceed, { push(boolean, pop(number) >= pop(number)); })
external_function(equalorexceed,  { push(boolean, pop(number) <= pop(number)); })

external_function(number_, {push(boolean, pop_any().type == number);}) // Question marks are converted to underscores.
external_function(listp_, {push(boolean, pop_any().type == list);})    // However all other synbols are too.
external_function(string_, {push(boolean, pop_any().type == string);}) // So this is a temporary hack!
external_function(block_, {push(boolean, pop_any().type == block);})

external_function(discard, { 
  // O(n) where n is the number of element being Discarded.
  double num = pop(number);
  int num_discarding = num;
  if (num != num_discarding) throw_error("Cannot Discard a non-integer number of elements!");
  if (num_discarding < 0) throw_error("Cannot Discard a negative number of elements!");
  cognate_object obj = pop_any();
  switch(obj.type)
  {
    case string: 
      for (int i = num_discarding-1; i >= 0; ++i)
        if (obj.string[i] == '\0') 
          throw_error("String is too small to Discard from!");
      push(string, obj.string + num_discarding); 
      break;
    case list: {
      cognate_list *lst = (cognate_list*)malloc(sizeof(cognate_list));
      *lst = *obj.list;
      if ((lst->start += num_discarding) > lst->top) throw_error("List is too small to Discard from!");
      push(list, lst);
    } break;
    default: type_error("List or String", lookup_type(obj.type));
  }
})

external_function(take, {
  // O(n) where n is the number of element being Taken.
  double num = pop(number);
  int num_taking = num;
  if (num != num_taking) throw_error("Cannot Take a non-integer number of elements!");
  if (num_taking < 0) throw_error("Cannot Take a negative number of elements!");
  cognate_object obj = pop_any();
  switch(obj.type)
  {
    case string: {
      for (int i = num_taking-1; i >= 0; --i)
        if (obj.string[i] == '\0') 
          throw_error("String is too small to Take from!");
      char* str = (char*) malloc (sizeof(char) * num_taking);
      strcpy(str, obj.string);
      str[num_taking] = '\0';
      push(string, str);
      break;
    }
    case list: {
      cognate_list *lst = (cognate_list*) malloc (sizeof(cognate_list));
      *lst = *obj.list;
      if (lst->start + num_taking > lst->top) throw_error("List is too small to Take from!");
      lst->top = lst->start + num_taking;
      push(list, lst);
    }
    break;
  default: type_error("List or String", lookup_type(obj.type));

  }
})

external_function(index, { 
  int index = pop(number);
  cognate_object obj = pop_any();
  switch(obj.type)
  {
    case string: 
      if (strlen(obj.string) <= index)
        throw_error("String index out of bounds!");
      char* str = (char*)malloc(sizeof(char));
      *str = obj.string[index];
      push(string, str);
      break;
    case list:
      if (obj.list->start + index > obj.list->top)
        throw_error("List index out of bounds!");
      push_any(obj.list->start [index]);
      break;
    default: type_error("List or String", lookup_type(obj.type)); break;
  }
})

external_function(length,{
  cognate_object obj = pop_any();
  switch(obj.type)
  {
    case string: push(number, (double)strlen(obj.string)); break;
    case list: push(number, (double)(obj.list->top - obj.list -> start)); break;
    default: type_error("List or String", lookup_type(obj.type)); break;
  }
})

external_function(list,  { 
  // I solemnly swear that I will NEVER RETURN THE ADDRESS OF A LOCAL VARIABLE!
  // Get the block argument
  void(^expr)(void) = pop(block);
  // Move the stack to temporary storage
  cognate_list temp_stack = stack;
  size_t temp_stack_size = stack_size;
  // Allocate a list as the stack
  init_stack();
  // Eval expr
  expr();
  // Store the resultant list, realloc-ing to fit snugly in memory.
  cognate_list* lst = (cognate_list*)malloc(sizeof(cognate_list));
  lst->start = (cognate_object*)realloc(stack.start, (stack.top - stack.start) * sizeof(cognate_object));
  lst->top = stack.top - stack.start + lst->start;
  //TODO: Shrink the list to fit here.
  // Restore the original stack
  stack = temp_stack;
  stack_size = temp_stack_size;
  // Push the created list to the stack
  push(list, lst);
})

external_function(characters, {
  char* str = pop(string);  
  cognate_list* lst = (cognate_list*)malloc(sizeof(cognate_list));
  lst->start = (cognate_object*)malloc(sizeof(cognate_object) * (strlen(str) + 1));
  lst->top = lst->start + strlen(str);
  for (int i = strlen(str); i >= 0; --i)
  {
    char *temp = (char*)malloc(sizeof(char));
    *temp = str[i];
    lst->start[i] = ((cognate_object){.type=string, .string=temp});
  }
  push(list, lst);
})

external_function(tuple,
{
  // Allocate the list object.
  cognate_list* lst = (cognate_list*)malloc(sizeof(cognate_list));
  // Initialise the list.
  lst->start = (cognate_object*)malloc(sizeof(cognate_object) * 2);
  lst->top = lst->start + 2;
  // Fill the list.
  lst->start[0] = pop_any();
  lst->start[1] = pop_any();
  // Push the list.
  push(list, lst);
})

external_function(stack,
{
  push(list, &stack);
})

external_function(if,
{
  pop(block)();
  if (pop(boolean)) 
  {
    void(^temp)(void) = pop(block);
    pop(block);
    temp();
  } 
  else 
  {
    pop(block);
    pop(block)();
  }
})

external_function(append,
{
  cognate_object obj1 = pop_any();
  cognate_object obj2 = pop_any();
  switch(obj1.type)
  {
    case string:
      if (obj2.type != string) type_error("String", lookup_type(obj2.type));
      {
        int list1_size = strlen(obj1.string);
        int list2_size = strlen(obj2.string);
        int new_list_size = list1_size + list2_size;
        char* str = (char*)malloc(sizeof(char) * new_list_size);
        strcpy(str, obj2.string);
        strcat(str, obj1.string);
        push(string, str);
      } 
      break;
    case list: 
      if (obj2.type != list) type_error("List", lookup_type(obj2.type));
      {
        int list1_len = obj1.list->top - obj1.list->start;
        int list2_len = obj2.list->top - obj2.list->start;
        int new_list_len = list1_len + list2_len;
        cognate_list *lst = (cognate_list*) malloc (sizeof(cognate_list));
        lst->start = (cognate_object*) malloc (sizeof(cognate_object) * new_list_len);
        lst->top = lst->start + new_list_len;
        memcpy(lst->start, obj2.list->start, list2_len * sizeof(cognate_object));
        memcpy(lst->start+list2_len, obj1.list->start, list1_len * sizeof(cognate_object));
        push(list, lst);
      }
      break;
    default: type_error("List or String", lookup_type(obj1.type)); 
  }
})

#endif
