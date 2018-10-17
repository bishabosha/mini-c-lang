package mycc

import Ast._

enum Types {
  case int, void, function
}

enum EqualityOperators {
  case EQUAL, NOT_EQUAL
}

enum RelationalOperators {
  case LT, GT, LT_EQ, GT_EQ
}

enum AdditiveOperators {
  case PLUS, MINUS
}

enum MultiplicativeOperators {
  case MULTIPLY, DIVIDE, MODULUS
}

enum UnaryOperators {
  case NOT, POSTIVE, NEGATIVE
}

enum StorageTypes {
  case auto, extern
}

enum ArgList {
  case LVoid, LAny
  case LParam(list: Vector[Parameter])
}

sealed trait Ast

object Ast {
  type Parameter = (Types, Identifier) | Types
  type Statements = Block | Declarations | Assignments | Return
  type Declarator = Identifier | FunctionDeclarator
  type InitDeclarator = Declarator | Assignment
  type DeclarationSpecifiers = Type | Storage
  type Declarations = Declaration | Function | Assignment
  type Expressions = List[Assignments]
  type Assignments = Assignment | Equalities 
  type Equalities = Equality | Relationals
  type Relationals = Relational | Additives
  type Additives = Additive | Multiplicatives
  type Multiplicatives = Multiplicative | Unaries
  type Unaries = Unary | Postfix
  type Postfix = Application | Primary
  type Primary = Identifier | Constant | StringLiteral | LazyExpressions
}

case class Identifier(id: String) extends Ast
case class FunctionDeclarator(name: Identifier, args: ArgList)
case class Constant(value: Int) extends Ast
case class StringLiteral(value: String) extends Ast
case class Type(id: Types) extends Ast
case class Storage(id: StorageTypes) extends Ast
case class Return(value: Expressions) extends Ast
case class Application(operand: Postfix, args: Expressions) extends Ast
case class LazyExpressions(value: Expressions) extends Ast
case class Unary(op: UnaryOperators, value: Unaries) extends Ast
case class Multiplicative(op: MultiplicativeOperators, left: Multiplicatives, right: Unaries) extends Ast
case class Additive(op: AdditiveOperators, left: Additives, right: Multiplicatives) extends Ast
case class Relational(op: RelationalOperators, left: Relationals, right: Additives) extends Ast
case class Equality(op: EqualityOperators, left: Equalities, right: Relationals) extends Ast
case class Assignment(lvalue: Identifier, rvalue: Assignments) extends Ast
case class Declaration(storage: StorageTypes, declType: Types, declarator: Declarator) extends Ast
case class Function(storage: StorageTypes, returnType: Types, declarator: FunctionDeclarator, body: List[Statements]) extends Ast
case class Block(inner: List[Statements]) extends Ast