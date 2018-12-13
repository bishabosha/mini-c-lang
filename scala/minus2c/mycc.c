#include "C.tab.h"
#include "ast.h"
#include <ctype.h>
#include <polyglot.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
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
void print_TokenString(TokenString *);
void print_TokenInt(TokenInt *);
void print_singleton(Ast *);

void print_level(int level) {
  int i;
  for (i = 0; i < level; i++) {
    putchar(' ');
  }
}

void print_ast(Ast *ast) { print_ast0(ast, 0); }

void print_ast0(Ast *ast, int level) {
  if (NULL == ast) {
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
    print_TokenString((TokenString *)ast);
    return;
  case TOKEN_INT:
    print_TokenInt((TokenInt *)ast);
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

void print_TokenString(TokenString *token) {
  switch (token->ast.type) {
  case STRING_LITERAL:
    printf("\"%s\"\n", token->lexeme);
    break;
  default:
    printf("%s\n", token->lexeme);
    break;
  }
}

void print_TokenInt(TokenInt *token) { printf("%d\n", token->value); }

void print_singleton(Ast *ast) { printf("%s\n", named(ast)); }

extern int yydebug;
extern int yyparse(Ast **);
extern void init_SymbTable(void);

void set_debug(bool debug) { yydebug = debug ? 1 : 0; }

Ast *get_ast() {
  Ast *ast = NULL;
  yyparse(&ast);
  return polyglot_from_ast(ast);
}

static bool mycc_CAst = false;
static void *(*mycc_CAst$Singleton)(void *);
static void *(*mycc_CAst$TokenInt)(void *, int);
static void *(*mycc_CAst$TokenString)(void *, void *);
static void *(*mycc_CAst$UnaryNode)(void *, void *);
static void *(*mycc_CAst$BinaryNode)(void *, void *, void *);
static void *(*mycc_CAst$Sequence)(void *, void *);
static void *(*System$out$println)(void *);
static void *(*immutable$List$empty)();
static void *(*ArrayDeque$new)();

#define SCALA_SINGLETON_FUNCTION(type)                                         \
  polyglot_get_member(polyglot_java_type(type), "apply")

void init_mycc_CAst() {

  mycc_CAst = true;
  mycc_CAst$Singleton = SCALA_SINGLETON_FUNCTION("mycc.CAst$Singleton");
  mycc_CAst$TokenInt = SCALA_SINGLETON_FUNCTION("mycc.CAst$TokenInt");
  mycc_CAst$TokenString = SCALA_SINGLETON_FUNCTION("mycc.CAst$TokenString");
  mycc_CAst$UnaryNode = SCALA_SINGLETON_FUNCTION("mycc.CAst$UnaryNode");
  mycc_CAst$BinaryNode = SCALA_SINGLETON_FUNCTION("mycc.CAst$BinaryNode");
  mycc_CAst$Sequence = SCALA_SINGLETON_FUNCTION("mycc.CAst$Sequence");
  immutable$List$empty = polyglot_get_member(
      polyglot_java_type("scala.collection.immutable.List"), "empty");
  System$out$println = polyglot_get_member(
      polyglot_get_member(polyglot_java_type("java.lang.System"), "out"),
      "println");
}

void *Ast_to_Scala(Ast *);
void *UnaryNode_to_Scala(UnaryNode *);
void *BinaryNode_to_Scala(BinaryNode *);
void *Sequence_to_Scala(Ast, BinaryNode *);
void *TokenInt_to_Scala(TokenInt *);
void *TokenString_to_Scala(TokenString *);
void *Singleton_to_Scala(Ast *);

void *acc_List(int, BinaryNode *);
void *Node_to_Scala(Ast *);
void *acc_stack(Ast *);

void print_java(void *obj) { System$out$println(obj); }

void *Ast_to_Scala(Ast *ast) {
  if (NULL == ast) {
    return NULL;
  }
  if (!mycc_CAst) {
    init_mycc_CAst();
  }
  switch (ast->tag) {
  case UNARY_NODE:
    return UnaryNode_to_Scala((UnaryNode *)ast);
  case BINARY_NODE:
    return BinaryNode_to_Scala((BinaryNode *)ast);
  case TOKEN_INT:
    return TokenInt_to_Scala((TokenInt *)ast);
  case TOKEN_STRING:
    return TokenString_to_Scala((TokenString *)ast);
  case SINGLETON:
    return Singleton_to_Scala(ast);
  default:
    return NULL;
  }
}

#define JAVA_STRING(expr) polyglot_from_string(expr, "UTF-8")

void *Sequence_to_Scala(Ast ast, BinaryNode *node) {
  void *list = acc_List(ast.type, node);
  return mycc_CAst$Sequence(JAVA_STRING(named(&ast)), list);
}

#define ARRAYDEQUE_NEW() polyglot_new_instance(polyglot_java_type("java.util.ArrayDeque"))
#define PUSH(obj, deque) polyglot_invoke(deque, "push", obj)
#define POP(deque) polyglot_invoke(deque, "pop")
#define IS_EMPTY(deque) polyglot_invoke(deque, "isEmpty")
#define CONS(obj, list) polyglot_invoke(list, "$colon$colon", obj)

void *acc_List(int type, BinaryNode *node) {
  void *list = immutable$List$empty();
  bool reverse = false;
  bool decided = false;
  Ast *left = node->a1;
  Ast *right = node->a2;
  while (true) {
    if (left->tag == BINARY_NODE && left->type == type) {
      if (!decided) {
        decided = true;
      }
      free(node);
      list = CONS(Ast_to_Scala(right), list);
      node = (BinaryNode *)left;
      left = node->a1;
      right = node->a2;
    } else if (right->tag == BINARY_NODE && right->type == type) {
      if (!decided) {
        decided = true;
        reverse = true;
      }
      free(node);
      list = CONS(Ast_to_Scala(left), list);
      node = (BinaryNode *)right;
      left = node->a1;
      right = node->a2;
    } else {
      free(node);
      if (reverse) {
        list = CONS(Ast_to_Scala(left), list);
        list = CONS(Ast_to_Scala(right), list);
      } else {
        list = CONS(Ast_to_Scala(right), list);
        list = CONS(Ast_to_Scala(left), list);
      }
      break;
    }
  }
  return reverse ? polyglot_invoke(list, "reverse") : list;
}

void *Node_to_Scala(Ast *ast) {
  return acc_stack(ast);
}

void *BinaryNode_to_Scala(BinaryNode *node) {
  switch (node->ast.type) {
  case 'E':
  case ';':
  case ',':
  case '~':
    return Sequence_to_Scala(node->ast, node);
  }
  void *scala =
      mycc_CAst$BinaryNode(JAVA_STRING(named(&node->ast)),
                           Ast_to_Scala(node->a1), Ast_to_Scala(node->a2));
  free(node);
  return scala;
}

void *UnaryNode_to_Scala(UnaryNode *node) {
  void *scala = mycc_CAst$UnaryNode(JAVA_STRING(named(&node->ast)),
                                    Ast_to_Scala(node->a1));
  free(node);
  return scala;
}

void *TokenString_to_Scala(TokenString *token) {
  void *scala = mycc_CAst$TokenString(JAVA_STRING(named(&token->ast)),
                                      JAVA_STRING(token->lexeme));
  free(token->lexeme);
  free(token);
  return scala;
}

void *TokenInt_to_Scala(TokenInt *token) {
  void *scala =
      mycc_CAst$TokenInt(JAVA_STRING(named(&token->ast)), token->value);
  free(token);
  return scala;
}

void *Singleton_to_Scala(Ast *ast) {
  return mycc_CAst$Singleton(JAVA_STRING(named(ast)));
}
