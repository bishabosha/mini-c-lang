#include "ast.h"
#include "C.tab.h"
#include <polyglot.h>
#include <stdio.h>
#include <stdlib.h>

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

Ast *UnaryNode_new(int t, Ast *left) {
  UnaryNode *a = (UnaryNode *)malloc(sizeof(UnaryNode));
  if (a == NULL) {
    perror("Cannot make node");
    exit(1);
  }
  a->ast.tag = UNARY_NODE;
  a->ast.type = t;
  a->left = left;
  return (Ast *)a;
}
