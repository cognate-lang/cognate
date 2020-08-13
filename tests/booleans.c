// Compiled from tests/booleans.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  push(block, ^{
    push(string, "FAIL: True AND True");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: True AND True");
    call(print);
  });
  push(block, ^{
    call(true);
    call(true);
    call(both);
    call(true);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: True AND False");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: True AND False");
    call(print);
  });
  push(block, ^{
    call(false);
    call(true);
    call(both);
    call(false);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: False AND True");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: False AND True");
    call(print);
  });
  push(block, ^{
    call(true);
    call(false);
    call(both);
    call(false);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: False AND False");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: False AND False");
    call(print);
  });
  push(block, ^{
    call(false);
    call(false);
    call(both);
    call(false);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: True OR True");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: True OR True");
    call(print);
  });
  push(block, ^{
    call(true);
    call(true);
    call(either);
    call(true);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: True OR False");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: True OR False");
    call(print);
  });
  push(block, ^{
    call(false);
    call(true);
    call(either);
    call(true);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: False OR True");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: False OR True");
    call(print);
  });
  push(block, ^{
    call(true);
    call(false);
    call(either);
    call(true);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: False OR False");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: False OR False");
    call(print);
  });
  push(block, ^{
    call(false);
    call(false);
    call(either);
    call(false);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: True XOR True");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: True XOR True");
    call(print);
  });
  push(block, ^{
    call(true);
    call(true);
    call(one_of);
    call(false);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: True XOR False");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: True XOR False");
    call(print);
  });
  push(block, ^{
    call(false);
    call(true);
    call(one_of);
    call(true);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: False XOR True");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: False XOR True");
    call(print);
  });
  push(block, ^{
    call(true);
    call(false);
    call(one_of);
    call(true);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "FAIL: False XOR False");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: False XOR False");
    call(print);
  });
  push(block, ^{
    call(false);
    call(false);
    call(one_of);
    call(false);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "PASS: NOT True");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: NOT True");
    call(print);
  });
  push(block, ^{
    call(true);
    call(not );
    call(false);
    call(equal);
  });
  call(if);
  push(block, ^{
    push(string, "PASS: NOT False");
    call(print);
  });
  push(block, ^{
    push(string, "PASS: NOT False");
    call(print);
  });
  push(block, ^{
    call(false);
    call(not );
    call(true);
    call(equal);
  });
  call(if);
  return 0;
}
