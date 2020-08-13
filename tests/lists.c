// Compiled from tests/lists.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  push(block, ^{
    push(number, 1);
    push(number, 2);
    push(number, 3);
  });
  call(list);
  variable(foo, immutable);
  {
    push(block, ^{
      push(number, 4);
      push(number, 5);
      push(number, 6);
      push(number, 7);
      push(number, 8);
    });
    call(list);
    variable(bar, immutable);
    {
      push(block, ^{
        push(number, 9);
        push(number, 10);
      });
      call(list);
      variable(baz, immutable);
      {
        push(block, ^{
          push(string, "FAIL: Comparing lists and tuples");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Comparing lists and tuples");
          call(print);
        });
        push(block, ^{
          call(baz);
          push(number, 10);
          push(number, 9);
          call(tuple);
          call(equal);
        });
        call(if);
        push(block, ^{
          push(string, "FAIL: Appending lists");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Appending lists");
          call(print);
        });
        push(block, ^{
          call(foo);
          call(bar);
          call(append);
          push(block, ^{
            push(number, 1);
            push(number, 2);
            push(number, 3);
            push(number, 4);
            push(number, 5);
            push(number, 6);
            push(number, 7);
            push(number, 8);
          });
          call(list);
          call(equal);
        });
        call(if);
        push(block, ^{
          push(string, "FAIL: Discarding from lists");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Discarding from lists");
          call(print);
        });
        push(block, ^{
          call(bar);
          push(number, 2);
          call(discard);
          push(block, ^{
            push(number, 6);
            push(number, 7);
            push(number, 8);
          });
          call(list);
          call(equal);
        });
        call(if);
        push(block, ^{
          push(string, "FAIL: Taking from lists");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: Taking from lists");
          call(print);
        });
        push(block, ^{
          call(bar);
          push(number, 2);
          call(take);
          push(block, ^{
            push(number, 4);
            push(number, 5);
          });
          call(list);
          call(equal);
        });
        call(if);
        push(block, ^{
          push(string, "FAIL: length of lists");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: length of lists");
          call(print);
        });
        push(block, ^{
          call(foo);
          call(length);
          push(number, 3);
          call(equal);
        });
        call(if);
        push(block, ^{
          push(string, "FAIL: indexing lists");
          call(print);
        });
        push(block, ^{
          push(string, "PASS: indexing lists");
          call(print);
        });
        push(block, ^{
          call(bar);
          push(number, 4);
          call(index);
          push(number, 8);
          call(equal);
        });
        call(if);
      }
    }
  }
  return 0;
}
