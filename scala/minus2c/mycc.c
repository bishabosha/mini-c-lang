#include "C.tab.h"
#include "ast.h"
#include <ctype.h>
#include <polyglot.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

char *named(Ast *ast) {
  int token = ast->type;
  static char b[100];
  switch (token) {
  case IDENTIFIER:
    return "id";
  case CONSTANT:
    return "constant";
  case STRING_LITERAL:
    return "string";
  case LE_OP:
    return "<=";
  case GE_OP:
    return ">=";
  case EQ_OP:
    return "==";
  case NE_OP:
    return "!=";
  case EXTERN:
    return "extern";
  case AUTO:
    return "auto";
  case INT:
    return "int";
  case VOID:
    return "void";
  case FUNCTION:
    return "function";
  case APPLY:
    return "apply";
  case LEAF:
    return "leaf";
  case IF:
    return "if";
  case ELSE:
    return "else";
  case WHILE:
    return "while";
  case CONTINUE:
    return "continue";
  case BREAK:
    return "break";
  case RETURN:
    return "return";
  case EMPTY:
    return "ø";
  default:
    if (isgraph(token) || token == ' ') {
      sprintf(b, "%c", token);
      return b;
    } else {
      return "???";
    };
  }
}

POLYGLOT_DECLARE_STRUCT(ast);
POLYGLOT_DECLARE_TYPE(Data);

extern void *get_SymbTable_inst();
void print_tree(Node *, int);
void print_binary_tree(BinaryNode *, int);
void print_ast0(Ast *, int);
void print_int_constant(Token *);
void print_string_constant(Token *);
void print_token_default(Token *token);
void print_token(Token *);

void print_level(int level) {
  int i;
  for (i = 0; i < level; i++) {
    putchar(' ');
  }
}

void print_ast(Ast *ast) { print_ast0(ast, 0); }

void print_ast0(Ast *ast, int level) {
  if (NULL == ast || polyglot_is_null(ast)) {
    return;
  }
  print_level(level);
  switch (ast->tag) {
  case NODE:
    print_tree((Node *)ast, level);
    return;
  case BINARY_NODE:
    print_binary_tree((BinaryNode *)ast, level);
    return;
  case TOKEN:
    print_token((Token *)ast);
    return;
  }
}

void print_tree(Node *node, int level) {
  printf("%s\n", named(&node->ast));
  print_ast0(node->left, level + 2);
}

void print_binary_tree(BinaryNode *node, int level) {
  printf("%s\n", named(&node->ast));
  print_ast0(node->left, level + 2);
  print_ast0(node->right, level + 2);
}

void print_token(Token *token) {
  switch (token->ast.type) {
  case STRING_LITERAL:
    print_string_constant(token);
    break;
  case CONSTANT:
    print_int_constant(token);
    break;
  default:
    print_token_default(token);
    break;
  }
}

void print_int_constant(Token *constant) {
  printf("%d\n", constant->data.value);
}

void print_string_constant(Token *token) {
  printf("\"%s\"\n", token->data.lexeme);
}

void print_token_default(Token *token) {
  printf("%s\n", token->data.lexeme);
}

extern int yydebug;
extern Ast *yyparse(void);
extern Ast *ans;
extern void init_symbtable(void);

void set_debug(bool debug) { yydebug = debug ? 1 : 0; }

Ast *get_ans() { return polyglot_from_ast(ans); }

static void *java_util_ArrayDeque;
static void *scala_util_Left_String;
static void *scala_util_Right_Int;

static void *mycc_CAst;
static void *mycc_CAst$Token;
static void *mycc_CAst$Node;
static void *mycc_CAst$BinaryNode;

static void *(*mycc_CAst$Token_new)(void *, void *);
static void *(*mycc_CAst$Node_new)(void *, void *);
static void *(*mycc_CAst$BinaryNode_new)(void *, void *, void *);

static void *(*java_util_ArrayDeque_push)(void *);
static void *(*java_util_ArrayDeque_pop)();

static void *(*scala_util_Left_String_new)(void *);
static void *(*scala_util_Right_Int_new)(int);

static void *java_util_ArrayDeque_inst;

void init_mycc_CAst() {
  java_util_ArrayDeque = polyglot_java_type("java.util.ArrayDeque");

  scala_util_Left_String = polyglot_java_type("scala.util.Left");
  scala_util_Right_Int = polyglot_java_type("scala.util.Right");

  mycc_CAst = polyglot_java_type("mycc.CAst");
  mycc_CAst$Token = polyglot_java_type("mycc.CAst$Token");
  mycc_CAst$BinaryNode = polyglot_java_type("mycc.CAst$BinaryNode");
  mycc_CAst$Node = polyglot_java_type("mycc.CAst$Node");

  mycc_CAst$Token_new = polyglot_get_member(mycc_CAst$Token, "apply");
  mycc_CAst$Node_new = polyglot_get_member(mycc_CAst$Node, "apply");
  mycc_CAst$BinaryNode_new = polyglot_get_member(mycc_CAst$BinaryNode, "apply");

  java_util_ArrayDeque_inst = polyglot_new_instance(java_util_ArrayDeque);
  java_util_ArrayDeque_pop =
      polyglot_get_member(java_util_ArrayDeque_inst, "pop");
  java_util_ArrayDeque_push =
      polyglot_get_member(java_util_ArrayDeque_inst, "push");

  scala_util_Left_String_new =
      polyglot_get_member(scala_util_Left_String, "apply");
  scala_util_Right_Int_new =
      polyglot_get_member(scala_util_Right_Int, "apply");
}

void Ast_to_Scala(Ast *);
void Node_to_Scala(Node *);
void BinaryNode_to_Scala(BinaryNode *);
void Token_to_Scala(Token *);

void *get_deque() { return java_util_ArrayDeque_inst; }

void Ast_to_Scala(Ast *ast) {
  if (NULL == ast || polyglot_is_null(ast)) {
    return;
  }
  if (NULL == mycc_CAst) {
    init_mycc_CAst();
  }
  switch (ast->tag) {
  case NODE:
    Node_to_Scala((Node *)ast);
    return;
  case BINARY_NODE:
    BinaryNode_to_Scala((BinaryNode *)ast);
    return;
  case TOKEN:
    Token_to_Scala((Token *)ast);
    return;
  }
}

void BinaryNode_to_Scala(BinaryNode *node) {
  Ast_to_Scala(node->right);
  Ast_to_Scala(node->left);
  java_util_ArrayDeque_push(polyglot_from_string(named(&node->ast), "UTF-8"));
  java_util_ArrayDeque_push(mycc_CAst$BinaryNode_new(
      java_util_ArrayDeque_pop(), java_util_ArrayDeque_pop(),
      java_util_ArrayDeque_pop()));
}

void Node_to_Scala(Node *node) {
  Ast_to_Scala(node->left);
  java_util_ArrayDeque_push(polyglot_from_string(named(&node->ast), "UTF-8"));
  java_util_ArrayDeque_push(mycc_CAst$Node_new(java_util_ArrayDeque_pop(),
                                               java_util_ArrayDeque_pop()));
}

void Token_to_Scala(Token *token) {
  switch (token->ast.type) {
  case CONSTANT:
    java_util_ArrayDeque_push(scala_util_Right_Int_new(token->data.value));
    java_util_ArrayDeque_push(
        polyglot_from_string(named(&token->ast), "UTF-8"));
    java_util_ArrayDeque_push(mycc_CAst$Token_new(java_util_ArrayDeque_pop(),
                                                  java_util_ArrayDeque_pop()));
    return;
  default:
    java_util_ArrayDeque_push(
        polyglot_from_string(token->data.lexeme, "UTF-8"));
    java_util_ArrayDeque_push(
        scala_util_Left_String_new(java_util_ArrayDeque_pop()));
    java_util_ArrayDeque_push(
        polyglot_from_string(named(&token->ast), "UTF-8"));
    java_util_ArrayDeque_push(mycc_CAst$Token_new(java_util_ArrayDeque_pop(),
                                                  java_util_ArrayDeque_pop()));
    return;
  }
}
