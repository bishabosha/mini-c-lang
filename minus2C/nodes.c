#include "nodes.h"
#include "C.tab.h"
#include "token.h"
#include <polyglot.h>
#include <stdio.h>
#include <stdlib.h>

POLYGLOT_DECLARE_STRUCT(node)

void* make_node(int t, void* left, void* right)
{
    NODE *a = (NODE*)malloc(sizeof(NODE));
    if (a==NULL) {
      perror("Cannot make node");
      exit(1);
    }
    a->type = t;
    a->left = left;
    a->right = right;
    return a;
}

void* make_leaf(void* l)
{
    NODE *a = (NODE*)malloc(sizeof(NODE));
/*     printf("make_leaf: %p -> %p\n", l, a); */
    if (a==NULL) {
      perror("Cannot make leaf");
      exit(1);
    }
    a->type = LEAF;
    a->left = l;
    a->right = NULL;
    return a;
}
