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
#include "token.h"
#include <polyglot.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nodes.h"

static void *SymbTable_inst;
static AST *(*SymbTable_get)(const void *);
static void (*SymbTable_put)(const void *, AST*);

static void *current;

TOKEN *int_token, *void_token, *function_token;

void init_symbtable(void)
{
  SymbTable_inst = polyglot_new_instance(polyglot_java_type("mycc.SymbTable"));
  SymbTable_get = polyglot_get_member(SymbTable_inst, "get");
  SymbTable_put = polyglot_get_member(SymbTable_inst, "put");

  int_token = new_token(INT);
  int_token->lexeme = "int";

  function_token = new_token(FUNCTION);
  function_token->lexeme = "function";
  
  void_token = new_token(VOID);
  void_token->lexeme = "void";
}

TOKEN* lookup_token(const char *s)
{
  current = SymbTable_get(polyglot_from_string(s, "UTF-8"));

  if (polyglot_is_null(current)) {
    TOKEN* new = new_token(IDENTIFIER);
    new->lexeme = (char *)malloc(1 + strlen(s));
    strcpy(new->lexeme, s);
    current = new;
    SymbTable_put(polyglot_from_string(s, "UTF-8"), current);
  }

  return current;
}

void * get_SymbTable_inst() {
  return SymbTable_inst;
}
