#ifndef _Ast_H_
#define _Ast_H_

typedef enum { SINGLETON, UNARY_NODE, BINARY_NODE, TOKEN_INT, TOKEN_STRING } AstTag;

typedef struct ast {
  AstTag tag;
  int tpe;
} Ast;

typedef struct unary_node {
  Ast ast;
  Ast *a1;
} UnaryNode;

typedef struct binary_node {
  Ast ast;
  Ast *a1;
  Ast *a2;
} BinaryNode;

typedef struct token_int {
  Ast ast;
  int value;
} TokenInt;

typedef struct token_string {
  Ast ast;
  char *lexeme;
} TokenString;

extern Ast *TokenString_new(int, char*);
extern Ast *Singleton_new(int);
Ast *UnaryNode_new(int, Ast *);
Ast *BinaryNode_new(int, Ast *, Ast *);

#endif
