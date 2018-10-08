#ifndef _Ast_H_
#define _Ast_H_

typedef enum { NODE, TOKEN } AstTag;

typedef struct ast {
  AstTag tag;
  int type;
} Ast;

typedef struct node {
  Ast ast;
  Ast *left;
  Ast *right;
} Node;

typedef union {
  char *lexeme;
  int value;
} Data;

typedef struct token {
  Ast ast;
  Data data;
} Token;

extern Ast *lexeme_Token_new(int, char*);
Ast *Node_new(int, Ast *, Ast *);

#endif