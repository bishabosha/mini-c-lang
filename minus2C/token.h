#ifndef __TOKEN_H
#define __TOKEN_H

#include "ast.h"

typedef struct TOKEN
{
  AST  ast;
  char *lexeme;
  int  value;
} TOKEN;

extern TOKEN* new_token(int);

#endif
