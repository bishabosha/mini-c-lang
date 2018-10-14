#include <stdio.h>

int a, b, c, d;
int foo() {
    a = 10, b = 50, c = b, d = 20;
}
int main(void) {
    foo(1, 2);
    return 0;
}
