#include "vector.h"
#include <stdio.h>
#include <stdlib.h>

void vector_init(Vector *vector) {
  // initialize size and capacity
  vector->size = 0;
  vector->capacity = VECTOR_INITIAL_CAPACITY;

  // allocate memory for vector->data
  vector->data = malloc(sizeof(Ast *) * vector->capacity);
}

bool vector_empty(Vector *vector) {
  return vector->size == 0;
}

void vector_push(Vector *vector, Ast* value) {
  // make sure there's room to expand into
  vector_double_capacity_if_full(vector);

  // append the value and increment vector->size
  vector->data[vector->size++] = value;
}

Ast* vector_pop(Vector *vector) {
  if (vector->size == 0) {
    printf("Index out of bounds for vector of size %d\n",
           vector->size);
    exit(1);
  }
  return vector->data[vector->size-- -1];
}

void vector_double_capacity_if_full(Vector *vector) {
  if (vector->size >= vector->capacity) {
    // double vector->capacity and resize the allocated memory accordingly
    vector->capacity *= 2;
    vector->data = realloc(vector->data, sizeof(Ast *) * vector->capacity);
  }
}

void vector_free(Vector *vector) { free(vector->data); }