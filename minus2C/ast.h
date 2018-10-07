#ifndef _AST_H_
#define _AST_H_

#include <stdbool.h>

typedef struct ast {
  bool is_token;
  int type;
} AST;

#endif