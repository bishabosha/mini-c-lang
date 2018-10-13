#include "C.tab.h"
#include "ast.h"
#include <polyglot.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void *SymbTable_inst;
static Ast *(*SymbTable_get)(const void *);
static void (*SymbTable_put)(const void *, Ast *);

static Ast *current;

Ast *int_token, *void_token, *function_token, *return_token, *break_token,
    *continue_token, *empty_token, *extern_token, *auto_token;

void init_SymbTable(void) {
  SymbTable_inst = polyglot_new_instance(polyglot_java_type("mycc.SymbTable"));
  SymbTable_get = polyglot_get_member(SymbTable_inst, "get");
  SymbTable_put = polyglot_get_member(SymbTable_inst, "put");

  int_token = Singleton_new(INT);
  function_token = Singleton_new(FUNCTION);
  void_token = Singleton_new(VOID);
  return_token = Singleton_new(RETURN);
  break_token = Singleton_new(BREAK);
  continue_token = Singleton_new(CONTINUE);
  empty_token = Singleton_new(EMPTY);
  extern_token = Singleton_new(EXTERN);
  auto_token = Singleton_new(AUTO);
}

Ast *get_identifier(const char *s) {
  current = SymbTable_get(polyglot_from_string(s, "UTF-8"));

  if (polyglot_is_null(current)) {
    char *lexeme = (char *)malloc(1 + strlen(s));
    strcpy(lexeme, s);

    Ast *new = TokenString_new(IDENTIFIER, lexeme);
    current = new;
    SymbTable_put(polyglot_from_string(s, "UTF-8"), current);
  }

  return (Ast *)current;
}

void *get_SymbTable_inst() { return SymbTable_inst; }
