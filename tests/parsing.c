// Compiled from tests/parsing.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  push(number, 5);
  variable(a, immutable);
  {
    push(block, ^{
      push(string, "FAIL: Comments");
      call(print);
    });
    push(block, ^{
      push(string, "PASS: Comments");
      call(print);
    });
    push(block, ^{
      call(a);
      push(number, 5);
      call(equal);
    });
    call(if);
    push(number, 5);
    variable(b, immutable);
    {
      push(string, " \" Let B be 4; \" ");
      push(string, " ' Let B be 4; ' ");
      call(clear);
      push(string, " Let B be 4; '  Let B be 4 ");
      push(string, "Let B be 4;");
      push(string, "");
      push(block, ^{
        push(string, "FAIL: Strings");
        call(print);
      });
      push(block, ^{
        push(string, "PASS: Strings");
        call(print);
      });
      push(block, ^{
        call(b);
        push(number, 5);
        call(equal);
      });
      call(if);
      push(block, ^{
        push(string, "FAIL: Floating point numbers");
        call(print);
      });
      push(block, ^{
        push(string, "PASS: Floating point numbers");
        call(print);
      });
      push(block, ^{
        push(number, 1.5);
        push(number, 3);
        push(number, 2);
        call(divisor);
        call(equal);
      });
      call(if);
    }
  }
  return 0;
}
