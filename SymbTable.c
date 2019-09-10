#include "C.tab.h"
#include "ast.h"
#include <polyglot.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void *SymbTable_inst;
static void *(*SymbTable_get)(const void *);
static void (*SymbTable_put)(const void *);
Ast *get(const char *s);

Ast *int_token, *void_token, *function_token, *return_token, *break_token,
    *continue_token, *empty_token, *extern_token, *auto_token, *empty_block_token;

void init_SymbTable(void) {
  SymbTable_inst = polyglot_new_instance(polyglot_java_type("mmc.SymbTable"));
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
  empty_block_token = Singleton_new('B');
}

Ast *get_identifier(const char *s) {
  if (!polyglot_as_boolean(SymbTable_get(polyglot_from_string(s, "UTF-8")))) {
    SymbTable_put(polyglot_from_string(s, "UTF-8"));
    return get(s);
  } else {
    return get(s);
  }
}

Ast *get(const char *s) {
  char *lexeme = (char *)malloc(1 + strlen(s));
  strcpy(lexeme, s);
  return TokenString_new(IDENTIFIER, lexeme);
}

void *get_SymbTable_inst() { return SymbTable_inst; }
