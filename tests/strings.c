// Compiled from tests/strings.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  push(block, ^{
    push(string, "FAIL: Appending strings");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Appending strings");
    call(print);
  });
  push(block, ^{
    push(string, "Hello");
    push(string, " world");
    call(append);
    push(string, "Hello world");
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Discarding from strings");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Discarding from strings");
    call(print);
  });
  push(block, ^{
    push(string, "Hello");
    push(number, 2);
    call(discard);
    push(string, "llo");
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: Taking from strings");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: Taking from strings");
    call(print);
  });
  push(block, ^{
    push(string, "Hello");
    push(number, 2);
    call(take);
    push(string, "He");
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: length of strings");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: length of strings");
    call(print);
  });
  push(block, ^{
    push(string, "Hello world");
    call(length);
    push(number, 11);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: indexing strings");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: indexing strings");
    call(print);
  });
  push(block, ^{
    push(string, "Hello world");
    push(number, 4);
    call(index);
    push(string, "o");
    call(equal);
  });
  call(if);
  return 0;
}
