#include "ast.h"
#include "C.tab.h"
#include <polyglot.h>
#include <stdio.h>
#include <stdlib.h>

Ast *BinaryNode_new(int t, Ast *a1, Ast *a2) {
  BinaryNode *a = (BinaryNode *)malloc(sizeof(BinaryNode));
  if (a == NULL) {
    perror("Cannot make node");
    exit(1);
  }
  a->ast.tag = BINARY_NODE;
  a->ast.type = t;
  a->a1 = a1;
  a->a2 = a2;
  return (Ast*)a;
}

Ast *UnaryNode_new(int t, Ast *a1) {
  UnaryNode *a = (UnaryNode *)malloc(sizeof(UnaryNode));
  if (a == NULL) {
    perror("Cannot make node");
    exit(1);
  }
  a->ast.tag = UNARY_NODE;
  a->ast.type = t;
  a->a1 = a1;
  return (Ast *)a;
}
