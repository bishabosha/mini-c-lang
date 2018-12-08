#include <polyglot.h>
#include <stdio.h>

static void *(*Optional$empty)();
static void *(*System$out$println)(void *);

void callee(void *);
void caller();

void init_foreign() {
  Optional$empty =
      polyglot_get_member(polyglot_java_type("java.util.Optional"), "empty");
  System$out$println = polyglot_get_member(
      polyglot_get_member(polyglot_java_type("java.lang.System"), "out"),
      "println");
}

void caller() {
  callee(Optional$empty());
}

void callee(void *obj) {
  System$out$println(obj);
}