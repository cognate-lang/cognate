// Compiled from tests/functions.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  function(foo, mutable, {
    variable(x, immutable);
    {
      push(number, 1);
      call(x);
      call(sum);
    }
  });
  {
    push(block, ^{
      push(string, "FAIL: Function call and return");
      call(print);
    });
    push(block, ^{
      push(string, "PASS: Function call and return");
      call(print);
    });
    push(block, ^{
      push(number, 4);
      call(foo);
      push(number, 5);
      call(equal);
    });
    call(if);
    {
      function(foo, immutable, {
        variable(x, immutable);
        {
          push(number, 2);
          call(x);
          call(sum);
        }
      });
      {
        push(block, ^{
          push(string, "FAIL: Function shadowing");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Function shadowing");
          call(print);
        });
        push(block, ^{
          push(number, 5);
          call(foo);
          push(number, 7);
          call(equal);
        });
        call(if);
      }
    }
    push(block, ^{
      push(string, "FAIL: Function shadowing in block");
      call(print);
    });
    push(block, ^{
      push(string, "PASS: Function shadowing in block");
      call(print);
    });
    push(block, ^{
      push(number, 7);
      call(foo);
      push(number, 8);
      call(equal);
    });
    call(if);
    {
      mutate_function(foo, {
        variable(x, immutable);
        {
          push(number, 3);
          call(x);
          call(sum);
        }
      });
      {
        push(block, ^{
          push(string, "FAIL: Function modification");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Function modification");
          call(print);
        });
        push(block, ^{
          push(number, 8);
          call(foo);
          push(number, 11);
          call(equal);
        });
        call(if);
      }
    }
    push(block, ^{
      push(string, "FAIL: Function modification in block");
      call(print);
    });
    push(block, ^{
      push(string, "PASS: Function modification in block");
      call(print);
    });
    push(block, ^{
      push(number, 11);
      call(foo);
      push(number, 14);
      call(equal);
    });
    call(if);
    call(false);
    variable(recurred, mutable);
    {
      function(bar, mutable, {
        variable(x, immutable);
        {
          push(block, ^{
            push(number, 0);
            call(bar);
          });
          push(block, ^{
            call(true);
            mutate_variable(recurred);
            {}
          });
          push(block, ^{
            call(x);
            push(number, 0);
            call(equal);
          });
          call(if);
        }
      });
      {
        push(number, 10);
        call(bar);
        push(block, ^{
          push(string, "FAIL: Recursion");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Recursion");
          call(print);
        });
        push(block, ^{
          call(recurred);
        });
        call(if);
      }
    }
  }
  return 0;
}
