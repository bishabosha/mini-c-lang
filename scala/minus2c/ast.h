#ifndef _Ast_H_
#define _Ast_H_

typedef enum { SINGLETON, NODE, BINARY_NODE, TOKEN } AstTag;

typedef struct ast {
  AstTag tag;
  int type;
} Ast;

typedef struct node {
  Ast ast;
  Ast *left;
} Node;

typedef struct binary_node {
  Ast ast;
  Ast *left;
  Ast *right;
} BinaryNode;

typedef union {
  char *lexeme;
  int value;
} Data;

typedef struct token {
  Ast ast;
  Data data;
} Token;

extern Ast *lexeme_Token_new(int, char*);
extern Ast *Singleton_new(int);
Ast *BinaryNode_new(int, Ast *, Ast *);
Ast *Node_new(int, Ast *);

#endif