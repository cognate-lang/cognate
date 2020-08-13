// Compiled from tests/variables.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  push(number, 1);
  variable(x, mutable);
  {
    push(block, ^{
      push(string, "FAIL: Variable assignment");
      call(print);
    });
    push(block, ^{
      push(string, "PASS: Variable assignment");
      call(print);
    });
    push(block, ^{
      call(x);
      push(number, 1);
      call(equal);
    });
    call(if);
    {
      push(number, 2);
      variable(x, immutable);
      {
        push(block, ^{
          push(string, "FAIL: Variable shadowing");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Variable shadowing");
          call(print);
        });
        push(block, ^{
          call(x);
          push(number, 2);
          call(equal);
        });
        call(if);
      }
    }
    push(block, ^{
      push(string, "FAIL: Variable shadowing in block");
      call(print);
    });
    push(block, ^{
      push(string, "PASS: Variable shadowing in block");
      call(print);
    });
    push(block, ^{
      call(x);
      push(number, 1);
      call(equal);
    });
    call(if);
    {
      push(number, 3);
      mutate_variable(x);
      {
        push(block, ^{
          push(string, "FAIL: Variable modification");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Variable modification");
          call(print);
        });
        push(block, ^{
          call(x);
          push(number, 3);
          call(equal);
        });
        call(if);
      }
    }
    push(block, ^{
      push(string, "FAIL: Variable modification in block");
      call(print);
    });
    push(block, ^{
      push(string, "PASS: Variable modification in block");
      call(print);
    });
    push(block, ^{
      call(x);
      push(number, 3);
      call(equal);
    });
    call(if);
  }
  return 0;
}
