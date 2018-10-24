(module
 (; --C source:
    int foo(int a, int b, int c) {
        return a + b + c;  
    }

    int main(void) {
        1, 2;
        int a = 3 + 4 == 2, b = ---1 % 3;
        a = a == 56;
        int c;
        return (3 == 2, c, foo(1, 2, 3));
    }
 ;)
 (table 0 anyfunc)
 (memory $0 1)
 (export "memory" (memory $0))
 (export "foo" (func $foo))
 (export "main" (func $main))
 (func $foo (; 0 ;) (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (i32.add
   (i32.add
    (get_local $1)
    (get_local $0)
   )
   (get_local $2)
  )
 )
 (func $main (; 1 ;) (result i32)
  (i32.const 6) (; evaluated foo to discover constant ;)
 )
)
