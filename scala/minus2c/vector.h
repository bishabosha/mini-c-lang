#ifndef _VECTOR_H_
#define _VECTOR_H_

#include "ast.h"
#include <stdbool.h>

#define VECTOR_INITIAL_CAPACITY 100

// Define a vector type
typedef struct {
  int size;     // slots used so far
  int capacity; // total available slots
  Ast **data;    // array of integers we're storing
} Vector;

void vector_init(Vector *vector);

bool vector_empty(Vector *vector);

void vector_push(Vector *vector, Ast* value);

Ast* vector_pop(Vector *vector);

void vector_double_capacity_if_full(Vector *vector);

void vector_free(Vector *vector);

#endif
