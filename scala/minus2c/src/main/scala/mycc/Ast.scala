package mycc

import mycc._
import Ast._
import EqualityOperators._
import RelationalOperators._
import AdditiveOperators._
import MultiplicativeOperators._

enum Types {
  case Cint, Cvoid, Cfunction
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
  case Auto, Extern
}

enum ArgList {
  case LVoid, LAny
  case LParam(list: Vector[Parameter])
}

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
  type Constants = Identifier | Constant | StringLiteral
  type Primary = Constants | LazyExpressions | Temporary
  type Node = Equality | Relational | Additive | Multiplicative | Unary
}

case class Temporary(rvalue: Assignments) {
  override def hashCode() = System.identityHashCode(this)
  override def equals(that: Any): Boolean =
    that match {
      case that: Temporary => that.canEqual(this) && (this eq that)
      case _ => false
  }
}

case class Identifier(id: String)
case class FunctionDeclarator(id: Identifier, args: ArgList)
case class Constant(value: Int)
case class StringLiteral(value: String)
case class Type(id: Types)
case class Storage(id: StorageTypes)
case class Return(value: Expressions)
case class Application(Operand: Postfix, args: Expressions)
case class LazyExpressions(value: Expressions)
case class Unary(op: UnaryOperators, value: Unaries)
case class Multiplicative(op: MultiplicativeOperators, left: Multiplicatives, right: Unaries)
case class Additive(op: AdditiveOperators, left: Additives, right: Multiplicatives)
case class Relational(op: RelationalOperators, left: Relationals, right: Additives)
case class Equality(op: EqualityOperators, left: Equalities, right: Relationals)
case class Assignment(lvalue: Identifier, rvalue: Assignments)
case class Declaration(storage: StorageTypes, declType: Types, declarator: Declarator)
case class Function(id: Identifier, frame: Frame, body: List[Statements])
case class Block(inner: List[Statements])

object Expression {
  
  type ExpressionRoot = Equality | Relational | Additive | Multiplicative |
    Unary | Constant | StringLiteral | Identifier | Relational

  def unapply(node: Assignments): Boolean =
    node match {
      case _: ExpressionRoot | Temporary(Expression()) =>
        true
      case _ =>
        false
    }
}

object IdentifierDeclaration {
  def unapply(decl: Declaration): Boolean =
    decl.declarator.isInstanceOf[Identifier]
}