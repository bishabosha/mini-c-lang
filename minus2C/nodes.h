#ifndef __NODES_H
#define __NODES_H

#include "token.h"
#include "ast.h"

typedef struct node {
  AST ast;
  AST *left;
  AST *right;
} NODE;

AST* make_node(int, AST*, AST*);

#endif
