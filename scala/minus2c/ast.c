#include "ast.h"
#include "C.tab.h"
#include <polyglot.h>
#include <stdio.h>
#include <stdlib.h>

POLYGLOT_DECLARE_STRUCT(node)

Ast *BinaryNode_new(int t, Ast *left, Ast *right) {
  BinaryNode *a = (BinaryNode *)malloc(sizeof(BinaryNode));
  if (a == NULL) {
    perror("Cannot make node");
    exit(1);
  }
  a->ast.tag = BINARY_NODE;
  a->ast.type = t;
  a->left = left;
  a->right = right;
  return (Ast*)a;
}

Ast *Node_new(int t, Ast *left) {
  Node *a = (Node *)malloc(sizeof(Node));
  if (a == NULL) {
    perror("Cannot make node");
    exit(1);
  }
  a->ast.tag = NODE;
  a->ast.type = t;
  a->left = left;
  return (Ast *)a;
}
