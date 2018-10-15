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
  case MULTIPLY, DIVIDE
}

enum UnaryOperators {
  case REF, NOT, POSTIVE, NEGATIVE, POINTER_ACCESS
}

enum StorageTypes {
  case auto, extern
}

enum ArgList {
  case LVoid, LAny
  case LParam(list: Vector[(Types, Identifier)])
}

sealed trait Ast

case class Identifier(id: String) extends Ast
case class Constant(value: Int) extends Ast
case class StringLiteral(value: String) extends Ast
case class Type(id: Types) extends Ast
case class Storage(id: StorageTypes) extends Ast
case class Return(value: Option[Ast]) extends Ast
case class Application(operand: Postfix, args: List[Ast]) extends Ast
case class LazyExpressions(value: Expressions) extends Ast
case class Unary(op: UnaryOperators, value: Unaries) extends Ast
case class Multiplicative(op: MultiplicativeOperators, left: Multiplicatives, right: Unaries) extends Ast
case class Additive(op: AdditiveOperators, left: Additives, right: Multiplicatives) extends Ast
case class Relational(op: RelationalOperators, left: Relationals, right: Additives) extends Ast
case class Equality(op: EqualityOperators, left: Equalities, right: Relationals) extends Ast
case class Assignment(lvalue: Identifier, rvalue: Ast) extends Ast
case class Declaration(storage: StorageTypes, declType: Types, name: Identifier) extends Ast
case class Function(storage: StorageTypes, returnType: Types, name: Identifier, args: ArgList, body: List[Ast]) extends Ast
case class Block(inner: List[Ast]) extends Ast

object Ast {
  type Expressions = List[Assignments]
  type Assignments = Assignment | Equalities 
  type Equalities = Equality | Relationals
  type Relationals = Relational | Additives
  type Additives = Additive | Multiplicatives
  type Multiplicatives = Multiplicative | Unaries
  type Unaries = Unary | Postfix
  type Postfix = Application | Primary
  type Primary = Identifier | Constant | StringLiteral | LazyExpressions


  inline def storageOf(kind: StorageTypes): Storage = Storage(kind).asInstanceOf[Storage]
  inline def typeOf(kind: Types): Type = Type(kind).asInstanceOf[Type]
  inline def identifierOf(id: String): Identifier = Identifier(id).asInstanceOf[Identifier]
  // inline def blockOf(statements: List[Ast]): Block = Block(statements).asInstanceOf[Block]
}