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
  printf("node: %s\n", named(&node->ast));
  print_ast0(node->left, level + 2);
}

void print_binary_tree(BinaryNode *node, int level) {
  printf("node: %s\n", named(&node->ast));
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
  printf("constant: %d\n", constant->data.value);
}

void print_string_constant(Token *token) {
  printf("string_lit: \"%s\"\n", token->data.lexeme);
}

void print_token_default(Token *token) {
  printf("lexeme: %s\n", token->data.lexeme);
}

extern int yydebug;
extern Ast *yyparse(void);
extern Ast *ans;
extern void init_symbtable(void);

void set_debug(bool debug) { yydebug = debug ? 1 : 0; }

Ast *get_ans() { return polyglot_from_ast(ans); }

static void *java_util_ArrayDeque;
static void *scala_Console;
static void *mycc_Ast;
static void *mycc_Ast$Token;
static void *mycc_Ast$Constant;
static void *mycc_Ast$Node;
static void *mycc_Ast$BinaryNode;

static void *(*mycc_Ast$Token_new)(void *, void *);
static void *(*mycc_Ast$Constant_new)(int);
static void *(*mycc_Ast$Node_new)(void *, void *);
static void *(*mycc_Ast$BinaryNode_new)(void *, void *, void *);

static void *(*java_util_ArrayDeque_push)(void *);
static void *(*java_util_ArrayDeque_pop)();
static void (*scala_Console_println)(void *);

static void *java_util_ArrayDeque_inst;
static void *currentAst;

void init_mycc_Ast() {
  scala_Console = polyglot_java_type("scala.Console");
  java_util_ArrayDeque = polyglot_java_type("java.util.ArrayDeque");
  mycc_Ast = polyglot_java_type("mycc.Ast");
  mycc_Ast$Constant = polyglot_java_type("mycc.Ast$Constant");
  mycc_Ast$Token = polyglot_java_type("mycc.Ast$Token");
  mycc_Ast$BinaryNode = polyglot_java_type("mycc.Ast$BinaryNode");
  mycc_Ast$Node = polyglot_java_type("mycc.Ast$Node");

  mycc_Ast$Token_new = polyglot_get_member(mycc_Ast$Token, "apply");
  mycc_Ast$Constant_new = polyglot_get_member(mycc_Ast$Constant, "apply");
  mycc_Ast$Node_new = polyglot_get_member(mycc_Ast$Node, "apply");
  mycc_Ast$BinaryNode_new = polyglot_get_member(mycc_Ast$BinaryNode, "apply");

  java_util_ArrayDeque_inst = polyglot_new_instance(java_util_ArrayDeque);
  java_util_ArrayDeque_pop =
      polyglot_get_member(java_util_ArrayDeque_inst, "pop");
  java_util_ArrayDeque_push =
      polyglot_get_member(java_util_ArrayDeque_inst, "push");
  scala_Console_println = polyglot_get_member(scala_Console, "println");
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
  if (NULL == mycc_Ast) {
    init_mycc_Ast();
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
  scala_Console_println(java_util_ArrayDeque_inst);
  java_util_ArrayDeque_push(mycc_Ast$BinaryNode_new(
      java_util_ArrayDeque_pop(), java_util_ArrayDeque_pop(),
      java_util_ArrayDeque_pop()));
  scala_Console_println(java_util_ArrayDeque_inst);
}

void Node_to_Scala(Node *node) {
  Ast_to_Scala(node->left);
  java_util_ArrayDeque_push(polyglot_from_string(named(&node->ast), "UTF-8"));
  scala_Console_println(java_util_ArrayDeque_inst);
  java_util_ArrayDeque_push(mycc_Ast$Node_new(java_util_ArrayDeque_pop(),
                                              java_util_ArrayDeque_pop()));
  scala_Console_println(java_util_ArrayDeque_inst);
}

void Token_to_Scala(Token *token) {
  switch (token->ast.type) {
  case CONSTANT:
    java_util_ArrayDeque_push(mycc_Ast$Constant_new(token->data.value));
    scala_Console_println(java_util_ArrayDeque_inst);
    return;
  default:
    java_util_ArrayDeque_push(
        polyglot_from_string(token->data.lexeme, "UTF-8"));
    scala_Console_println(java_util_ArrayDeque_inst);
    java_util_ArrayDeque_push(
        polyglot_from_string(named(&token->ast), "UTF-8"));
    scala_Console_println(java_util_ArrayDeque_inst);
    java_util_ArrayDeque_push(mycc_Ast$Token_new(java_util_ArrayDeque_pop(),
                                                 java_util_ArrayDeque_pop()));
    scala_Console_println(java_util_ArrayDeque_inst);
    return;
  }
}
