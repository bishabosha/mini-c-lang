#ifndef _Ast_H_
#define _Ast_H_

typedef enum { NODE, TOKEN, STRING_CONSTANT, INT_CONSTANT } Tag;

typedef struct ast {
  Tag tag;
} Ast;

typedef struct node {
  Ast ast;
  int type;
  Ast *left;
  Ast *right;
} Node;

typedef struct token {
  Ast ast;
  char *lexeme;
} Token;

typedef struct string_constant {
  Token token;
} StringConstant;

typedef struct int_constant {
  Ast ast;
  int value;
} IntConstant;

extern Ast *Token_new(int, const char*);
Ast *Node_new(int, Ast *, Ast *);

#endif