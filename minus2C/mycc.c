#include <stdio.h>
#include <ctype.h>
#include "nodes.h"
#include "C.tab.h"
#include <string.h>
#include <stdbool.h>
#include <polyglot.h>

char *named(int t)
{
    static char b[100];   
    switch (t) {
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
      if (isgraph(t) || t == ' ') {
        sprintf(b, "%c", t);
        return b;
      } else {
        return "???";
      };
    }
}

POLYGLOT_DECLARE_STRUCT(node);

POLYGLOT_DECLARE_STRUCT(TOKEN);

void print_leaf(void *token, int level)
{
  TOKEN *t = token;
  int i;
  for (i = 0; i < level; i++)
    putchar(' ');
  if (t->type == CONSTANT) {
    printf("%d\n", t->value);
  } else if (t->type == STRING_LITERAL) {
    printf("\"%s\"\n", t->lexeme);
  } else if (t) {
    printf("%s\n", t->lexeme);
  }
}

void print_tree0(NODE *tree, int level)
{
  int i;
  if (tree == NULL || polyglot_is_null(tree)) {
    return;
  } else if (tree->type == LEAF) {
    print_leaf(tree->left, level);
  } else {
    for(i=0; i<level; i++) putchar(' ');
    printf("%s\n", named(tree->type));
    /*       if (tree->type=='~') { */
    /*         for(i=0; i<level+2; i++) putchar(' '); */
    /*         printf("%p\n", tree->left); */
    /*       } */
    /*       else */
    NODE* left = tree->left;
    if (left != NULL && !polyglot_is_null(left)) {
      int left_type = ((NODE*)left)->type;
      if (left_type == LEAF) {
        void *left_left = left->left;
        print_leaf(left_left, level + 2);
      } else {
        print_tree0(left, level + 2);
      }
    }

    NODE *right = tree->right;
    if (right != NULL && !polyglot_is_null(right)) {
      int right_type = ((NODE *)right)->type;
      if (right_type == LEAF) {
        void *tok = right->left;
        print_leaf(tok, level + 2);
      } else {
        print_tree0(right, level + 2);
      }
    }
  }
}

void print_tree(NODE *tree)
{
    print_tree0(tree, 0);
}

extern int yydebug;
extern NODE* yyparse(void);
extern NODE* ans;
extern void init_symbtable(void);

void set_debug(bool debug) {
    yydebug = debug ? 1 : 0;
}

void* get_ans() {
    return polyglot_from_node(ans);
}
