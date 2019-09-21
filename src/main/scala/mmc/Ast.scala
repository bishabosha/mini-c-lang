package mmc

import Constants._

object Ast {

  type Selections             = IfElse
  type Statements             = Block | Declarations | Assignments | Return | Selections
  type InitDeclarator         = Declarator | Assignment
  type DeclarationSpecifiers  = Type | Storage
  type Declarations           = Declaration | Function | Assignment
  type Expressions            = List[Assignments]
  type Literals               = Scoped | Constant
  type Primary                = Literals | LazyExpressions | Temporary
  type Postfix                = Primary | Application
  type Unaries                = Postfix | Unary
  type Multiplicatives        = Unaries | Multiplicative
  type Additives              = Multiplicatives | Additive
  type Relationals            = Additives | Relational
  type Equalities             = Relationals | Equality
  type Assignments            = Equalities | Assignment
  type Node                   = Equality | Relational | Additive | Multiplicative | Unary

  type ExpressionRoot         = Equality | Relational | Additive | Multiplicative |
                                  Unary | Constant | Variable

  case class Constant(value: StringLiteral | IntLiteral)
  case class Type(id: Types)
  case class Storage(id: StorageTypes)
  case class Return(value: Expressions)
  case class Application(operand: Postfix, args: Expressions)
  case class LazyExpressions(value: Expressions)
  case class Unary(op: UnaryOperators, value: Unaries)
  case class Multiplicative(op: MultiplicativeOperators, left: Multiplicatives, right: Unaries)
  case class Additive(op: AdditiveOperators, left: Additives, right: Multiplicatives)
  case class Relational(op: RelationalOperators, left: Relationals, right: Additives)
  case class Equality(op: EqualityOperators, left: Equalities, right: Relationals)
  case class Assignment(lvalue: Variable, rvalue: Assignments)
  case class Function(id: Scoped, frame: Frame, body: List[Statements])
  case class Block(inner: List[Statements])
  case class IfElse(id: Long, test: Expressions, ifThen: List[Statements], orElse: List[Statements])

  def temporaryAssignment(value: Assignments): Assignment =
    Assignment(new Temporary, value)

}
