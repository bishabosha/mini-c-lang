#include <stdio.h>

int foo(void) {
    // int a = 10;
    // int b = 20;
    // int c = 30;
    // a, b, c = 50;

    int a, b;
    int c = 50;
    printf("a: %d", a);
    printf("b: %d", b);
    printf("c: %d", c);
    return a, b, c;
}

int main(void) {
    printf("foo: %d", foo());

    return 0;
}
