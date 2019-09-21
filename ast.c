#include "ast.h"
#include "C.tab.h"
#include <polyglot.h>
#include <stdio.h>
#include <stdlib.h>

Ast *UnaryNode_new(int tpe, Ast *a1) {
  UnaryNode *a = (UnaryNode *)malloc(sizeof(UnaryNode));
  a->ast.tag = UNARY_NODE;
  a->ast.tpe = tpe;
  a->a1 = a1;
  return (Ast *)a;
}

Ast *BinaryNode_new(int tpe, Ast *a1, Ast *a2) {
  BinaryNode *a = (BinaryNode *)malloc(sizeof(BinaryNode));
  a->ast.tag = BINARY_NODE;
  a->ast.tpe = tpe;
  a->a1 = a1;
  a->a2 = a2;
  return (Ast*)a;
}
