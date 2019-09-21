#include "C.tab.h"
#include "ast.h"
#include <ctype.h>
#include <polyglot.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *named(Ast *ast) {
  int token = ast->type;
  static char b[100];
  switch (token) {
  case IDENTIFIER:
    return "id";
  case CONSTANT:
    return "constant";
  case STRING_LITERAL:
    return "string";
  case LE_OP:
    return "<=";
  case GE_OP:
    return ">=";
  case EQ_OP:
    return "==";
  case NE_OP:
    return "!=";
  case EXTERN:
    return "extern";
  case AUTO:
    return "auto";
  case INT:
    return "int";
  case VOID:
    return "void";
  case FUNCTION:
    return "function";
  case APPLY:
    return "apply";
  case IF:
    return "if";
  case ELSE:
    return "else";
  case WHILE:
    return "while";
  case CONTINUE:
    return "continue";
  case BREAK:
    return "break";
  case RETURN:
    return "return";
  case EMPTY:
    return "ø";
  default:
    if (isgraph(token) || token == ' ') {
      sprintf(b, "%c", token);
      return b;
    } else {
      return "???";
    };
  }
}

POLYGLOT_DECLARE_STRUCT(ast);
POLYGLOT_DECLARE_STRUCT(unary_node);
POLYGLOT_DECLARE_STRUCT(binary_node);
POLYGLOT_DECLARE_STRUCT(ternary_node);
POLYGLOT_DECLARE_STRUCT(token_int);
POLYGLOT_DECLARE_STRUCT(token_string);

#define JAVA_STRING(expr) polyglot_from_string(expr, "UTF-8")

extern void *get_SymbTable_inst();
extern void init_SymbTable(void);
void *ast_to_poly(Ast *ast);
TernaryNode *ast_to_ternary(Ast *ast);

extern int yydebug;
extern int yyparse(Ast **);

void set_debug(bool debug) { yydebug = debug ? 1 : 0; }

void **get_ast() {
  Ast *ast = NULL;
  yyparse(&ast);
  return ast_to_poly(ast);
}

void *ast_to_poly(Ast *ast) {
  switch (ast->tag) {
  case UNARY_NODE:
    return polyglot_from_unary_node((UnaryNode *)ast);
  case BINARY_NODE:
    return polyglot_from_binary_node((BinaryNode *)ast);
  case TERNARY_NODE:
    return polyglot_from_ternary_node((TernaryNode *)ast);
  case TOKEN_STRING:
    return polyglot_from_token_string((TokenString *)ast);
  case TOKEN_INT:
    return polyglot_from_token_int((TokenInt *)ast);
  case SINGLETON:
    return polyglot_from_ast(ast);
  }
}

TernaryNode *ast_to_ternary(Ast *ast) {
  switch (ast->tag) {
  case TERNARY_NODE:
    return (TernaryNode *)ast;
  case UNARY_NODE:
  case BINARY_NODE:
  case TOKEN_STRING:
  case TOKEN_INT:
  case SINGLETON:
    perror("Not ternary");
    exit(1);
  }
}

AstTag get_tag(Ast *ast) { return ast->tag; }

void *get_type(Ast *ast) { return JAVA_STRING(named(ast)); }

void *get_lexeme(TokenString *ast) { return JAVA_STRING(ast->lexeme); }

int get_value(TokenInt *ast) { return ast->value; }

void free_pointer(void *any) { free(any); }
