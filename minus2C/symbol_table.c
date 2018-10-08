/*
 * Adapted from 
 * CM20029 Coursework Assignment 1
 * Tom Crick
 * cs1tc@bath.ac.uk
 * 30 Apr 2003
 *
 * symbol_table.c
 */

#include "C.tab.h"
#include <polyglot.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"

static void *SymbTable_inst;
static Ast *(*SymbTable_get)(const void *);
static void (*SymbTable_put)(const void *, Ast*);

static void *current;

Token *int_token, *void_token, *function_token;

void init_symbtable(void)
{
  SymbTable_inst = polyglot_new_instance(polyglot_java_type("mycc.SymbTable"));
  SymbTable_get = polyglot_get_member(SymbTable_inst, "get");
  SymbTable_put = polyglot_get_member(SymbTable_inst, "put");

  int_token = Token_new(INT, "int");
  function_token = Token_new(FUNCTION, "function");
  void_token = Token_new(VOID, "void");
}

Token *Token_symbol_get(const char *s) {
  current = SymbTable_get(polyglot_from_string(s, "UTF-8"));

  if (polyglot_is_null(current)) {
    char *lexeme = (char *)malloc(1 + strlen(s));
    strcpy(lexeme, s);
    Token *new = Token_new(IDENTIFIER, lexeme);
    current = new;
    SymbTable_put(polyglot_from_string(s, "UTF-8"), current);
  }

  return current;
}

void * get_SymbTable_inst() {
  return SymbTable_inst;
}
