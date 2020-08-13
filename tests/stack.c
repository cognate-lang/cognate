// Compiled from tests/stack.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  push(block, ^{
    push(string, "FAIL: Drop");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Drop");
    call(print);
  });
  push(block, ^{
    push(number, 2);
    push(number, 3);
    call(drop);
    push(number, 2);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Swap");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Swap");
    call(print);
  });
  push(block, ^{
    push(number, 5);
    push(number, 6);
    call(swap);
    push(number, 5);
    call(equal);
  });
  call(if);
  call(drop);
  push(block, ^{
    push(string, "FAIL: Twin");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Twin");
    call(print);
  });
  push(block, ^{
    push(number, 4);
    call(twin);
    call(drop);
    push(number, 4);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Triplet");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Triplet");
    call(print);
  });
  push(block, ^{
    push(number, 7);
    call(triplet);
    call(drop);
    call(drop);
    push(number, 7);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Stack as list");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Stack as list");
    call(print);
  });
  push(block, ^{
    push(number, 1);
    push(number, 2);
    push(number, 3);
    call(stack);
    push(block, ^{
      push(number, 1);
      push(number, 2);
      push(number, 3);
    });
    call(list);
    call(equal);
  });
  call(if);
  return 0;
}
