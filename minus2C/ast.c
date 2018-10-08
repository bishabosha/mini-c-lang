#include "ast.h"
#include "C.tab.h"
#include <polyglot.h>
#include <stdio.h>
#include <stdlib.h>

POLYGLOT_DECLARE_STRUCT(node)

Ast *make_node(int t, Ast *left, Ast *right) {
  Node *a = (Node*)malloc(sizeof(Node));
  if (a == NULL) {
    perror("Cannot make node");
    exit(1);
  }
  a->ast.tag = NODE;
  a->type = t;
  a->left = left;
  a->right = right;
  return (Ast*)a;
}
