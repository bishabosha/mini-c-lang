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

static void *SymbTable;
static void *Console;
static void *(*Console_println)(const char *);
static void *(*SymbTable_get)(const char *);
static void (*SymbTable_put)(const char *, void*);

static void *current;
static TOKEN *current_tok;

// static TOKEN **symbtable;
// #define HASH_SIZE (1000)
TOKEN *int_token, *void_token, *function_token;

void init_symbtable(void)
{
  SymbTable = polyglot_java_type("mycc.SymbTable");
  SymbTable_get = polyglot_get_member(SymbTable, "get");
  SymbTable_put = polyglot_get_member(SymbTable, "put");

  Console = polyglot_java_type("scala.Console");
  Console_println = polyglot_get_member(Console, "println");

  Console_println(polyglot_from_string("hello world", "UTF-8"));

  int_token = new_token(INT);
  int_token->lexeme = "int";

  function_token = new_token(FUNCTION);
  function_token->lexeme = "function";
  
  void_token = new_token(VOID);
  void_token->lexeme = "void";
}

POLYGLOT_DECLARE_STRUCT(TOKEN);

void* lookup_token(const char *s)
{
    current = SymbTable_get(polyglot_from_string(s, "UTF-8"));

    if (polyglot_is_null(current)) {
      TOKEN* new = new_token(IDENTIFIER);
      new->lexeme = (char *)malloc(1 + strlen(s));
      strcpy(new->lexeme, s);
      SymbTable_put(polyglot_from_string(s, "UTF-8"), polyglot_from_TOKEN(new));
  } else {
    printf("ELSE EXEC\n");
    return current;
  }
}
