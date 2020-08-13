// Compiled from tests/maths.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  push(block, ^{
    push(string, "FAIL: Addition");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Addition");
    call(print);
  });
  push(block, ^{
    push(number, 6);
    push(number, 5);
    call(sum);
    push(number, 11);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Subtraction");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Subtraction");
    call(print);
  });
  push(block, ^{
    push(number, 19);
    push(number, 10);
    call(difference);
    push(number, 9);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Multiplication");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Multiplication");
    call(print);
  });
  push(block, ^{
    push(number, 8);
    push(number, 9);
    call(product);
    push(number, 72);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Division");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Division");
    call(print);
  });
  push(block, ^{
    push(number, 42);
    push(number, 7);
    call(divisor);
    push(number, 6);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Modulus");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Modulus");
    call(print);
  });
  push(block, ^{
    push(number, 10);
    push(number, 3);
    call(modulo);
    push(number, 1);
    call(equal);
  });
  call(if);
  return 0;
}
