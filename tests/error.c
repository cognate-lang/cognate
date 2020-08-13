// Compiled from tests/error.cog by CognaC version 0.0.1
#include "cognate.c"
int main() {
  init();
  push(string, "FAIL");
  call(print);
  return 0;
}
