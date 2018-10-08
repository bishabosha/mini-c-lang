#include "C.tab.h"
#include "ast.h"
#include <ctype.h>
#include <polyglot.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

char *named(Ast* ast)
{
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
    case APPLY:
      return "apply";
    case LEAF:
      return "leaf";
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

extern void *get_SymbTable_inst();
void print_tree(Node *, int);
void print_ast0(Ast *, int);
void print_int_constant(Token *);
void print_string_constant(Token *);
void print_token_default(Token *token);
void print_token(Token *);

void print_level(int level) {
  int i;
  for (i = 0; i < level; i++) {
    putchar(' ');
  }
}

void print_ast(Ast *ast) { print_ast0(ast, 0); }

void print_ast0(Ast *ast, int level) {
  if (NULL == ast || polyglot_is_null(ast)) {
    return;
  }
  print_level(level);
  switch(ast->tag) {
  case NODE:
    print_tree((Node *)ast, level);
    break;
  case TOKEN:
    print_token((Token *)ast);
    break;
  }
}

void print_tree(Node *node, int level) {
  printf("%s\n", named(&node->ast));
  print_ast0(node->left, level + 2);
  print_ast0(node->right, level + 2);
}

void print_token(Token *token) {
  switch (token->ast.type) {
  case STRING_LITERAL:
    print_string_constant(token);
    break;
  case CONSTANT:
    print_int_constant(token);
    break;
  default:
    print_token_default(token);
    break;
  }
}

void print_int_constant(Token *constant) {
  printf("%d\n", constant->data.value);
}

void print_string_constant(Token *token) { printf("\"%s\"\n", token->data.lexeme); }

void print_token_default(Token *token) { printf("%s\n", token->data.lexeme); }

extern int yydebug;
extern Ast *yyparse(void);
extern Ast *ans;
extern void init_symbtable(void);

void set_debug(bool debug) { yydebug = debug ? 1 : 0; }

Ast *get_ans() { return polyglot_from_ast(ans); }
