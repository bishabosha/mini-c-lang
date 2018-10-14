#include <stdio.h>

int a, b, c, d;
int foo(void) {
    a = 10, b = 50, c = b, d = 20;
}
int main(void) {
    a;
    return 0;
}
