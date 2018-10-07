#ifndef __NODES_H
#define __NODES_H

#include "token.h"

typedef struct node {
  int          type;
  void *left;
  void *right;
} NODE;

NODE* make_leaf(void*);
NODE* make_node(int, void*, void*);

#endif
