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
    if (isgraph(t) || t==' ') {
      sprintf(b, "%c", t);
      return b;
    }
    switch (t) {
      default: return "???";
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
    }
}

POLYGLOT_DECLARE_STRUCT(node);

POLYGLOT_DECLARE_STRUCT(TOKEN);

void print_leaf(void *token, int level)
{

  // printf("snap\n");
  TOKEN *t = token;

  // printf("crackle\n");
  int i;
  for (i = 0; i < level; i++)
    putchar(' ');

  // printf("pop\n");
  if (t->type == CONSTANT) {
    // printf("do\n");
    printf("%d\n", t->value);
    // printf("dis\n");
  } else if (t->type == STRING_LITERAL) {
    // printf("errday\n");
    printf("\"%s\"\n", t->lexeme);
    // printf("my\n");
  } else if (t) {
    // printf("dude\n");
    printf("%s\n", t->lexeme);
  }

}

void print_tree0(NODE *tree, int level)
{
    int i;
    if (polyglot_is_null(tree)) return;
    if (tree->type==LEAF) {
      print_leaf(tree->left, level);
    }
    else {
      for(i=0; i<level; i++) putchar(' ');
      printf("%s\n", named(tree->type));
      /*       if (tree->type=='~') { */
      /*         for(i=0; i<level+2; i++) putchar(' '); */
      /*         printf("%p\n", tree->left); */
      /*       } */
      /*       else */
      // printf("right\n");
      NODE* left = tree->left;
      // printf("itWorkd\n");
      if (polyglot_is_null(left) == false) {
        // printf("hmmAgain\n");
        int left_type = ((NODE*)left)->type;
        // printf("wot_is\n");
        if (left_type == LEAF) {
          // printf("dayum\n");
          void *left_left = left->left;

          // printf("hunny\n");
          print_leaf(left_left, level + 2);
        } else {
          // printf("aww\n");
          print_tree0(left, level + 2);
        }
      }

      // printf("tbh\n");
      NODE *right = tree->right;
      if (polyglot_is_null(right) == false) {
        // printf("wow\n");
        int right_type = ((NODE *)right)->type;
        // printf("phew\n");
        if (right_type == LEAF) {
          // printf("ok\n");
          void *tok = right->left;
          // printf("hmm\n");
          print_leaf(tok, level + 2);
        } else {
          // printf("dummy\n");
          print_tree0(right, level + 2);
        }
      }

      // printf("eighth\n");
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
