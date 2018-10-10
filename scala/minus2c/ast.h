#ifndef _Ast_H_
#define _Ast_H_

typedef enum { SINGLETON, UNARY_NODE, BINARY_NODE, TOKEN_INT, TOKEN_STRING } AstTag;

typedef struct ast {
  AstTag tag;
  int type;
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

typedef struct token {
  Ast ast;
  union {
    char *lexeme;
    int value;
  } data;
} Token;

extern Ast *Token_string_new(int, char*);
extern Ast *Singleton_new(int);
Ast *BinaryNode_new(int, Ast *, Ast *);
Ast *UnaryNode_new(int, Ast *);

#endif