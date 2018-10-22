package mycc

import mycc._
import Ast._
import EqualityOperators._
import RelationalOperators._
import AdditiveOperators._
import MultiplicativeOperators._

enum Types {
  case int, void, function
}

import scala.language.implicitConversions

trait Operand(val symbol: String)
trait BinaryOp(val op: (Int, Int) => Int)
trait UnaryOp(val op: Int => Int)

enum EqualityOperators(op: (Int, Int) => Int, symbol: String) extends Operand(symbol) with BinaryOp(op) {
  case EQUAL      extends EqualityOperators(_==_, "==")
  case NOT_EQUAL  extends EqualityOperators(_!=_, "!=")
}

enum RelationalOperators(op: (Int, Int) => Int, symbol: String) extends Operand(symbol) with BinaryOp(op) {
  case LT     extends RelationalOperators(_<_, "<")
  case GT     extends RelationalOperators(_>_, ">")
  case LT_EQ  extends RelationalOperators(_<=_, "<=")
  case GT_EQ  extends RelationalOperators(_>=_, ">=")
}

enum AdditiveOperators(op: (Int, Int) => Int, symbol: String) extends Operand(symbol) with BinaryOp(op) {
  case PLUS  extends AdditiveOperators(_+_, "+")
  case MINUS extends AdditiveOperators(_-_, "-")
}

enum MultiplicativeOperators(op: (Int, Int) => Int, symbol: String) extends Operand(symbol) with BinaryOp(op) {
  case MULTIPLY extends MultiplicativeOperators(_*_, "*")
  case DIVIDE   extends MultiplicativeOperators(_/_, "/")
  case MODULUS  extends MultiplicativeOperators(_%_, "%")
}

enum UnaryOperators(op: Int => Int, symbol: String) extends Operand(symbol) with UnaryOp(op) {
  case NOT      extends UnaryOperators(_ == 0, "!")
  case POSITIVE  extends UnaryOperators(identity, "+")
  case NEGATIVE extends UnaryOperators(_ * -1, "-")
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
  type Key = Identifier
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
  type Constants = Identifier | Constant | StringLiteral
  type Primary = Constants | LazyExpressions | Temporary
}

case class Temporary(rvalue: Assignments) extends Ast {
  override def hashCode() = System.identityHashCode(this)
  override def equals(that: Any): Boolean =
    that match {
      case that: Temporary => that.canEqual(this) && (this eq that)
      case _ => false
  }
}

case class Identifier(id: String) extends Ast
case class FunctionDeclarator(id: Identifier, args: ArgList)
case class Constant(value: Int) extends Ast
case class StringLiteral(value: String) extends Ast
case class Type(id: Types) extends Ast
case class Storage(id: StorageTypes) extends Ast
case class Return(value: Expressions) extends Ast
case class Application(Operand: Postfix, args: Expressions) extends Ast
case class LazyExpressions(value: Expressions) extends Ast
case class Unary(op: UnaryOperators, value: Unaries) extends Ast
case class Multiplicative(op: MultiplicativeOperators, left: Multiplicatives, right: Unaries) extends Ast
case class Additive(op: AdditiveOperators, left: Additives, right: Multiplicatives) extends Ast
case class Relational(op: RelationalOperators, left: Relationals, right: Additives) extends Ast
case class Equality(op: EqualityOperators, left: Equalities, right: Relationals) extends Ast
case class Assignment(lvalue: Identifier, rvalue: Assignments) extends Ast
case class Declaration(storage: StorageTypes, declType: Types, declarator: Declarator) extends Ast
case class Function(id: Identifier, body: List[Statements]) extends Ast
case class Block(inner: List[Statements]) extends Ast