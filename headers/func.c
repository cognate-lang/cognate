#ifndef FUNC_C
#define FUNC_C

#include "stack.c"
#include "type.c"
#include "io.c"
#include <Block.h>

// Macro to define external cognate function.
#define cognate_func(name, body) \
  void cognate_func_ ## name () body

// Macro for defining external cognate variables with specified type.
#define cognate_var(name, type, value) \
  void cognate_func_ ## name () { \
    push(type, value); }

// Macro for defining external cognate variables from raw cognate_objects.
#define cognate_var_obj(name, value) \
  static void cognate_func_ ## name() { \
    push_object(value); }

#ifdef DEBUG
  #define call(name) {fprintf(stderr, "[DEBUG]%s:%d -> Calling %s\n", __FILE__, __LINE__, #name); /*check_recursion_depth();*/ cognate_func_##name();}
#else 
  #define call(name) {/*check_recursion_depth();*/ cognate_func_##name();}
#endif

#define malloc GC_MALLOC
#define realloc GC_REALLOC


cognate_func(do,             { pop(block)();                             })
cognate_func(print,          { print_object(pop_object()); puts(""); })

cognate_func(sum,            { push(number, pop(number) + pop(number));               })
cognate_func(product,        { push(number, pop(number) * pop(number));               })
cognate_func(divisor,        { double n = pop(number); push(number, pop(number) / n); })
cognate_func(difference,     { double n = pop(number); push(number, pop(number) - n); })
cognate_func(modulo,         { int n = pop(number); push(number, (int)pop(number) % n); }) // TODO: add checking if integer.

cognate_func(random,         { double low = pop(number); double high = pop(number); double step = pop(number); 
                               push(number, low + (double)(rand() % (int)((high - low) / step)) * step); })

cognate_func(drop,           { pop_object();                                                                                     })
cognate_func(clone,          { push_object(peek_object());                                                                       })
cognate_func(swap,           { cognate_object a = pop_object(); cognate_object b = pop_object(); push_object(a); push_object(b); })
cognate_func(clear,          { init_stack();                                                                                     })

cognate_var(true,  block, ^{ call(swap); call(drop); })
cognate_var(false, block, ^{ call(drop);             })


cognate_func(equal,          { if (pop(number) == pop(number)) call(true) else call(false); })
cognate_func(notequal,       { if (pop(number) != pop(number)) call(true) else call(false); })
cognate_func(preceed,        { if (pop(number) >  pop(number)) call(true) else call(false); })
cognate_func(exceed,         { if (pop(number) <  pop(number)) call(true) else call(false); })
cognate_func(equalorpreceed, { if (pop(number) >= pop(number)) call(true) else call(false); })
cognate_func(equalorexceed,  { if (pop(number) <= pop(number)) call(true) else call(false); })

void cognate_func_tail() { 
  cognate_list *lst = (cognate_list*)malloc(sizeof(cognate_list));
  *lst = *pop(list);
  if (lst->start == lst->top) 
    throw_error("Tail encountered empty list!");
  lst->start++;
  push(list, lst);
}

cognate_func(index, { 
  int index = pop(number);
  cognate_list lst = *pop(list);
  if (lst.start + index > lst.top)
    throw_error("Index out of bounds!");
  push_object(lst . start [index]);
})
cognate_func(length,{
  cognate_list* lst = pop(list);
  push(number, lst -> top - lst -> start);
})
cognate_func(list,  { 
  // I solemnly swear that I will NEVER RETURN THE ADDRESS OF A LOCAL VARIABLE!
  // Get the block argument
  void(^expr)(void) = pop(block);
  // Move the stack to temporary storage
  cognate_list temp_stack = stack;
  size_t temp_stack_size = stack_size;
  size_t temp_stack_size_next = stack_size_next;
  // Allocate a list as the stack
  init_stack();
  // Eval expr
  expr();
  // Store the resultant list
  cognate_list* lst = (cognate_list*)malloc(sizeof(stack));
  *lst = stack;
  //TODO: Shrink the list to fit here.
  // Restore the original stack
  stack = temp_stack;
  stack_size = temp_stack_size;
  stack_size_next = temp_stack_size_next;
  // Push the created list to the stack
  push(list, lst);
})

cognate_func(tuple,
{
  // Allocate the list object.
  cognate_list* lst = (cognate_list*)malloc(sizeof(cognate_list));
  // Initialise the list.
  *lst = (cognate_list){ .start = (cognate_object*)malloc(sizeof(cognate_object) * 2) };
  lst->top = lst->start + 2;
  // Fill the list.
  lst->start[0] = pop_object();
  lst->start[1] = pop_object();
  // Push the list.
  push(list, lst);
})


#endif
