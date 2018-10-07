#include <stdio.h>
#include <ctype.h>
#include "nodes.h"
#include "C.tab.h"
#include <string.h>
#include <stdbool.h>
#include <polyglot.h>

char *named(AST* ast)
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
void print_tree(AST *, int);
void print_leaf(AST *, int);
void print_ast0(AST *, int);

void print_level(int level) {
  int i;
  for (i = 0; i < level; i++) {
    putchar(' ');
  }
}

void print_ast(AST *ast) { print_ast0(ast, 0); }

void print_ast0(AST *ast, int level) {
  if (NULL == ast || polyglot_is_null(ast)) {
    return;
  }
  print_level(level);
  if (ast->is_token) {
    print_leaf(ast, level);
  } else {
    print_tree(ast, level);
  }
}

void print_leaf(AST *ast, int level) {
  TOKEN *token = (TOKEN *)ast;
  if (ast->type == CONSTANT) {
    printf("%d\n", token->value);
  } else if (ast->type == STRING_LITERAL) {
    printf("\"%s\"\n", token->lexeme);
  } else if (token) {
    printf("%s\n", token->lexeme);
  }
}

void print_tree(AST *ast, int level) {
  NODE *node = (NODE *)ast;
  printf("%s\n", named(ast));
  print_ast0(node->left, level + 2);
  print_ast0(node->right, level + 2);
}

extern int yydebug;
extern AST* yyparse(void);
extern AST* ans;
extern void init_symbtable(void);

void set_debug(bool debug) {
    yydebug = debug ? 1 : 0;
}

AST* get_ans() {
    return polyglot_from_ast(ans);
}
