#include "nodes.h"
#include "C.tab.h"
#include "token.h"
#include <polyglot.h>
#include <stdio.h>
#include <stdlib.h>

POLYGLOT_DECLARE_STRUCT(node)

AST *make_node(int t, AST *left, AST *right) {
  NODE *a = (NODE *)malloc(sizeof(NODE));
  if (a == NULL) {
    perror("Cannot make node");
    exit(1);
  }
  a->ast.is_token = false;
  a->ast.type = t;
  a->left = left;
  a->right = right;
  return (AST*)a;
}
