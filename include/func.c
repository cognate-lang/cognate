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

#ifdef DEBUG
  #define call(name) {fprintf(stderr, "[DEBUG]%s:%d -> Calling %s\n", __FILE__, __LINE__, #name); /*check_recursion_depth();*/ cognate_function_ ## name();}
#else 
  #define call(name) {/*check_recursion_depth();*/ cognate_function_ ## name();}
#endif

#define malloc GC_MALLOC
#define realloc GC_REALLOC

external_function(do,             { pop(block)();                             })
external_function(print,          { print_object(pop_any()); puts(""); })

external_function(sum,            { push(number, pop(number) + pop(number));               })
external_function(product,        { push(number, pop(number) * pop(number));               })
external_function(divisor,        { double n = pop(number); push(number, pop(number) / n); })
external_function(difference,     { double n = pop(number); push(number, pop(number) - n); })
external_function(modulo,         { int n = pop(number); push(number, (double)((int)pop(number) % n)); }) // TODO: add checking if integer.

external_function(random,         { double low = pop(number); double high = pop(number); double step = pop(number); 
                               push(number, low + (double)(rand() % (int)((high - low) / step)) * step); })

external_function(drop,           { pop_any();                                                                                     })
external_function(clone,          { push_object(peek_object());                                                                       })
external_function(swap,           { cognate_object a = pop_any(); cognate_object b = pop_any(); push_object(a); push_object(b); })
external_function(clear,          { init_stack();                                                                                     })

external_variable(true,  block, ^{ call(swap); call(drop); })
external_variable(false, block, ^{ call(drop);             })


external_function(equal,          { if (pop(number) == pop(number)) call(true) else call(false); })
external_function(notequal,       { if (pop(number) != pop(number)) call(true) else call(false); })
external_function(preceed,        { if (pop(number) >  pop(number)) call(true) else call(false); })
external_function(exceed,         { if (pop(number) <  pop(number)) call(true) else call(false); })
external_function(equalorpreceed, { if (pop(number) >= pop(number)) call(true) else call(false); })
external_function(equalorexceed,  { if (pop(number) <= pop(number)) call(true) else call(false); })

external_function(tail, { 
  cognate_list *lst = (cognate_list*)malloc(sizeof(cognate_list));
  *lst = *pop(list);
  if (lst->start == lst->top) 
    throw_error("Tail encountered empty list!");
  lst->start++;
  push(list, lst);
})

external_function(index, { 
  int index = pop(number);
  cognate_list lst = *pop(list);
  if (lst.start + index > lst.top)
    throw_error("Index out of bounds!");
  push_object(lst . start [index]);
})
external_function(length,{
  cognate_list* lst = pop(list);
  push(number, (double)(lst -> top - lst -> start));
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
  cognate_list* lst = (cognate_list*)malloc(sizeof(stack));
  lst->start = (cognate_object*) realloc(stack.start, (stack.top - stack.start) * sizeof(cognate_object));
  lst->top = stack.top - stack.start + lst->start;
  //TODO: Shrink the list to fit here.
  // Restore the original stack
  stack = temp_stack;
  stack_size = temp_stack_size;
  // Push the created list to the stack
  push(list, lst);
})

external_function(tuple,
{
  // Allocate the list object.
  cognate_list* lst = (cognate_list*)malloc(sizeof(cognate_list));
  // Initialise the list.
  *lst = (cognate_list){ .start = (cognate_object*)malloc(sizeof(cognate_object) * 2) };
  lst->top = lst->start + 2;
  // Fill the list.
  lst->start[0] = pop_any();
  lst->start[1] = pop_any();
  // Push the list.
  push(list, lst);
})

external_function(stack,
{
  push(list, &stack));
}

#endif
