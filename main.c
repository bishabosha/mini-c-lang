#include <stdio.h>

int foo(void) {
    int a = 10, b = 50, c = b, d = 20;
}

int main(void) {
    printf("foo: %d", foo());

    return 0;
}
