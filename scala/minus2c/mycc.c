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

extern void *get_SymbTable_inst();
void print_tree(UnaryNode *, int);
void print_binary_tree(BinaryNode *, int);
void print_ast0(Ast *, int);
void print_int_constant(Token *);
void print_string_constant(Token *);
void print_token_default(Token *);
void print_token_string(Token *);
void print_token_int(Token *);
void print_singleton(Ast *);

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
  case UNARY_NODE:
    print_tree((UnaryNode *)ast, level);
    return;
  case BINARY_NODE:
    print_binary_tree((BinaryNode *)ast, level);
    return;
  case TOKEN_STRING:
    print_token_string((Token *)ast);
    return;
  case TOKEN_INT:
    print_token_int((Token *)ast);
    return;
  case SINGLETON:
    print_singleton(ast);
  }
}

void print_tree(UnaryNode *node, int level) {
  printf("%s\n", named(&node->ast));
  print_ast0(node->a1, level + 2);
}

void print_binary_tree(BinaryNode *node, int level) {
  printf("%s\n", named(&node->ast));
  print_ast0(node->a1, level + 2);
  print_ast0(node->a2, level + 2);
}

void print_token_string(Token *token) {
  switch (token->ast.type) {
  case STRING_LITERAL:
    print_string_constant(token);
    break;
  default:
    print_token_default(token);
    break;
  }
}

void print_token_int(Token *constant) {
  printf("%d\n", constant->data.value);
}

void print_string_constant(Token *token) {
  printf("\"%s\"\n", token->data.lexeme);
}

void print_token_default(Token *token) { printf("%s\n", token->data.lexeme); }

void print_singleton(Ast *ast) { printf("%s\n", named(ast)); }

extern int yydebug;
extern Ast *yyparse(void);
extern Ast *ans;
extern void init_symbtable(void);

void set_debug(bool debug) { yydebug = debug ? 1 : 0; }

Ast *get_ans() { return polyglot_from_ast(ans); }

static void *mycc_CAst;
static void *(*mycc_CAst$Singleton_new)(void *);
static void *(*mycc_CAst$Token_new)(void *, void *);
static void *(*mycc_CAst$UnaryNode_new)(void *, void *);
static void *(*mycc_CAst$BinaryNode_new)(void *, void *, void *);

static void *(*ArrayDeque_push)(void *);
static void *(*ArrayDeque_pop)();

static void *(*scala_util_Left_String_new)(void *);
static void *(*scala_util_Right_Int_new)(int);

static void *ArrayDeque_inst;

void init_mycc_CAst() {

  mycc_CAst = polyglot_java_type("mycc.CAst");
  mycc_CAst$Singleton_new =
      polyglot_get_member(polyglot_java_type("mycc.CAst$Singleton"), "apply");
  mycc_CAst$Token_new =
      polyglot_get_member(polyglot_java_type("mycc.CAst$Token"), "apply");
  mycc_CAst$UnaryNode_new =
      polyglot_get_member(polyglot_java_type("mycc.CAst$UnaryNode"), "apply");
  mycc_CAst$BinaryNode_new =
      polyglot_get_member(polyglot_java_type("mycc.CAst$BinaryNode"), "apply");

  ArrayDeque_inst =
      polyglot_new_instance(polyglot_java_type("java.util.ArrayDeque"));
  ArrayDeque_pop = polyglot_get_member(ArrayDeque_inst, "pop");
  ArrayDeque_push = polyglot_get_member(ArrayDeque_inst, "push");

  scala_util_Left_String_new =
      polyglot_get_member(polyglot_java_type("scala.util.Left"), "apply");
  scala_util_Right_Int_new =
      polyglot_get_member(polyglot_java_type("scala.util.Right"), "apply");
}

void Ast_to_Scala(Ast *);
void UnaryNode_to_Scala(UnaryNode *);
void BinaryNode_to_Scala(BinaryNode *);
void Token_int_to_Scala(Token *);
void Token_string_to_Scala(Token *);
void Singleton_to_Scala(Ast *);

void *get_deque() { return ArrayDeque_inst; }

void Ast_to_Scala(Ast *ast) {
  if (NULL == ast || polyglot_is_null(ast)) {
    return;
  }
  if (NULL == mycc_CAst) {
    init_mycc_CAst();
  }
  switch (ast->tag) {
  case UNARY_NODE:
    UnaryNode_to_Scala((UnaryNode *)ast);
    return;
  case BINARY_NODE:
    BinaryNode_to_Scala((BinaryNode *)ast);
    return;
  case TOKEN_INT:
    Token_int_to_Scala((Token *)ast);
    return;
  case TOKEN_STRING:
    Token_string_to_Scala((Token *)ast);
    return;
  case SINGLETON:
    Singleton_to_Scala(ast);
    return;
  }
}

#define JAVA_STRING(expr) polyglot_from_string(expr, "UTF-8")

void BinaryNode_to_Scala(BinaryNode *node) {
  Ast_to_Scala(node->a2);
  Ast_to_Scala(node->a1);
  ArrayDeque_push(mycc_CAst$BinaryNode_new(JAVA_STRING(named(&node->ast)),
                                           ArrayDeque_pop(), ArrayDeque_pop()));
}

void UnaryNode_to_Scala(UnaryNode *node) {
  Ast_to_Scala(node->a1);
  ArrayDeque_push(mycc_CAst$UnaryNode_new(JAVA_STRING(named(&node->ast)),
                                          ArrayDeque_pop()));
}

void Token_string_to_Scala(Token *token) {
  ArrayDeque_push(mycc_CAst$Token_new(
      JAVA_STRING(named(&token->ast)),
      scala_util_Left_String_new(JAVA_STRING(token->data.lexeme))));
}

void Token_int_to_Scala(Token *token) {
  ArrayDeque_push(
      mycc_CAst$Token_new(JAVA_STRING(named(&token->ast)),
                          scala_util_Right_Int_new(token->data.value)));
}

void Singleton_to_Scala(Ast *ast) {
  ArrayDeque_push(mycc_CAst$Singleton_new(JAVA_STRING(named(ast))));
}
