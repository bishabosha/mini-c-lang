#include <stdio.h>

int foo() {
    int a = 1 + bar() * 2 + baz(2, 3);
    return a;
}

int bar() {
  return 0;
}

int baz(int a, int b) {
    return a + b;
}
