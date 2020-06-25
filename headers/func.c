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
  #define call(name) {puts("CALLING: "#name); cognate_func_##name();}
#else 
  #define call(name) cognate_func_##name();
#endif

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
  cognate_list* lst = pop(list);
  if (lst->start == lst->top) 
    throw_error("Tail encountered empty list!");
  static cognate_list lst2;
  lst2 = (cognate_list){.start = lst->start + 1, .top = lst->top};
  push(list, &lst2);
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

  // All list-related errors spawn from this one function.
  // One function to bring them all,
  // And in the darkness bind them.

  // Get the block argument
  void(^expr)(void) = pop(block); 
  // Move the stack to temporary storage
  cognate_list temp_stack = stack;
  cognate_object* temp_stack_end = stack_end;
  // Allocate a list as the stack
  init_stack();
  // Eval expr
  expr();
  // Store the resultant list
  cognate_list lst = stack;
  // Restore the original stack
  stack = temp_stack;
  stack_end = temp_stack_end;
  // Push the created list to the stack
  // Maybe lst should be realloced here because it will be larger than it needs to be.
  push(list, &lst);
})

#endif
