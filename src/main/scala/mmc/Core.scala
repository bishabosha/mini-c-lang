package mmc

import language.implicitConversions

type Parameter  = (Type, Scoped) | Type
type Variable   = Scoped | Temporary
type Declarator = Scoped | FunctionDeclarator
type Identifier = String

class Temporary
case class Scoped(id: Identifier, scope: Long)
case class FunctionDeclarator(id: Scoped, args: ArgList)

case class Declaration(storage: StorageKind, declType: Type, declarator: Declarator)

enum ArgList derives Eql
  case LVoid
  case LAny
  case LParam(list: List[Parameter])

object Constants
  opaque type StringLiteral = String
  opaque type IntLiteral    = Int

  def IntLiteral(i: Int): IntLiteral = i
  def StringLiteral(s: String): StringLiteral = s

  given (c: IntLiteral)
    def value: Int = c

  given (s: StringLiteral)
    def value: String = s

enum Type derives Eql
  case Cint, Cvoid, Cfunction, Cstring

enum StorageKind derives Eql
  case Auto, Extern

trait Operand(val symbol: String)
trait BinaryOp(val op: (Int, Int) => Int)
trait UnaryOp(val op: Int => Int)

type BinaryOperators = EqualityOperators | RelationalOperators | AdditiveOperators | MultiplicativeOperators

enum EqualityOperators(op: (Int, Int) => Int, symbol: String) extends Operand(symbol) with BinaryOp(op) derives Eql
  case EQUAL     extends EqualityOperators(_==_, "==")
  case NOT_EQUAL extends EqualityOperators(_!=_, "!=")

enum RelationalOperators(op: (Int, Int) => Int, symbol: String) extends Operand(symbol) with BinaryOp(op) derives Eql
  case LT    extends RelationalOperators(_<_, "<")
  case GT    extends RelationalOperators(_>_, ">")
  case LT_EQ extends RelationalOperators(_<=_, "<=")
  case GT_EQ extends RelationalOperators(_>=_, ">=")

enum AdditiveOperators(op: (Int, Int) => Int, symbol: String) extends Operand(symbol) with BinaryOp(op) derives Eql
  case PLUS  extends AdditiveOperators(_+_, "+")
  case MINUS extends AdditiveOperators(_-_, "-")

enum MultiplicativeOperators(op: (Int, Int) => Int, symbol: String) extends Operand(symbol) with BinaryOp(op) derives Eql
  case MULTIPLY extends MultiplicativeOperators(_*_, "*")
  case DIVIDE   extends MultiplicativeOperators(_/_, "/")
  case MODULUS  extends MultiplicativeOperators(_%_, "%")

enum UnaryOperators(op: Int => Int, symbol: String) extends Operand(symbol) with UnaryOp(op) derives Eql
  case NOT      extends UnaryOperators(_ == 0, "!")
  case POSITIVE extends UnaryOperators(identity, "+")
  case NEGATIVE extends UnaryOperators(_ * -1, "-")

object Std
  import ArgList._
  import StorageKind._
  import Type._

  val mainIdentifier    = Scoped("main",0)
  val mainIdentifierKey = DeclarationKey(Std.mainIdentifier.id)
  val mainDefinitionKey = DefinitionKey(Std.mainIdentifier)

  val mainFunc =
    Declaration(
      Auto,
      Cint,
      FunctionDeclarator(mainIdentifier, LVoid)
    )

  val printIntIdentifier = Scoped("print_int",0)
  val readIntIdentifier  = Scoped("read_int",0)

  val print_int =
    Declaration(
      Extern,
      Cvoid,
      FunctionDeclarator(
        printIntIdentifier,
        LParam(List(Cint -> Scoped("value",0)))
      )
    )

  val read_int =
    Declaration(
      Extern,
      Cint,
      FunctionDeclarator(
        readIntIdentifier,
        LVoid
      )
    )

  val declarations = Seq(print_int, read_int)
