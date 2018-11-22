package object mycc {
  import Ast._
  import StorageTypes._
  import Types._
  import parseCAst._
  import exception.SemanticError
  import exception.UnexpectedAstNode
  import ArgList._
  import MIPS._

  import scala.language.implicitConversions
  implicit def bool2Int(b: Boolean): Int = if b then 1 else 0

  case class DeclarationKey(id: Identifier) extends Bindings.Key {
    type Value = Declaration
  }

  case class DefinitionKey(id: Identifier) extends Bindings.Key {
    type Value = Function
  }

  case class RegisterKey(key: Identifier | Temporary) extends Bindings.Key {
    type Value = Register
  }

  case class DataKey(key: Label) extends Bindings.Key {
    type Value = Constant
  }

  def extractDeclarations
    (declarations: List[Declaration]
    ): Map[Bindings.Key, Any] =
      (for (d @ Declaration(_, _, decl) <- declarations)
        yield (DeclarationKey(extractIdentifier(decl)), d)
      ).toMap

  private def extractIdentifier(d: Declarator): Identifier = d match {
    case i: Identifier => i
    case _ @ FunctionDeclarator(i, _) => i
  }

  def replaceIdent(decl: Declaration, id: Identifier) = decl match {
    case Declaration(s,t, _: Identifier) =>
      Declaration(s,t,id)
    case Declaration(s,t,FunctionDeclarator(_,args)) =>
      Declaration(s,t,FunctionDeclarator(id,args))
  }

  def renameMain
    ( bindings: Bindings,
      id: Identifier,
      body: List[Statements],
      tac: List[Statements]
    ): (Bindings, List[Statements]) = {
      val newMainDecl = replaceIdent(Std.mainFunc, id)
      val newBindings = rename(
        Std.mainIdentifier,
        id,
        newMainDecl,
        body,
        bindings
      )
      val newCode =
        renameFunction(tac)(id,Std.mainIdentifier)
      (newBindings,newCode)
    }

  def renameFunction
    ( tac: List[Statements] )
    ( id: Identifier,
      old: Identifier,
    ): List[Statements] = tac.foldRight(Nil: List[Statements]) { (s,acc) =>
      s match {
        case Function(`old`, body) =>
          Function(id, body) :: acc
        case any => any :: acc
      }
    }

  def rename
    ( old: Identifier,
      id: Identifier,
      decl: Declaration,
      body: List[Statements],
      bindings: Bindings
    ): Bindings =
      declareIn(id, decl) {
        defineIn(id, Function(id,body)) {
          undefineIn(old) {
            undeclareIn(old) {
              bindings
            }
          }
        }
      }

  def unexpected(lvalue: Identifier | Temporary): Nothing = {
    val lStr = showLValue(lvalue)
    throw UnexpectedAstNode(s"unknown variable $lStr")
  }
  
  def showLValue(lvalue: Identifier | Temporary): String =
    lvalue match {
      case Identifier(id) => id
      case t: Temporary => ("_" + t.hashCode).take(6)
    }

  private def defineIn
    (key: Identifier, value: Function)
    (bindings: => Bindings): Bindings =
      bindings + (DefinitionKey(key), value)

  private def undefineIn
    (key: Identifier)
    (bindings: => Bindings): Bindings =
      bindings - DefinitionKey(key)

  private def declareIn
    (key: Identifier, value: Declaration)
    (bindings: => Bindings): Bindings =
      bindings + (DeclarationKey(key), value)

  private def undeclareIn
    (key: Identifier)
    (bindings: => Bindings): Bindings =
      bindings - DeclarationKey(key)

  object Std {
    val mainIdentifier = Identifier("main")
    val mainIdentifierKey = DeclarationKey(Std.mainIdentifier)
    val mainDefinitionKey = DefinitionKey(Std.mainIdentifier)
    val mainFunc =
    Declaration(
      Auto,
      Cint,
      FunctionDeclarator(mainIdentifier, LVoid)
    )
    val declarations = List[Declaration](
      Declaration(
        Extern,
        Cvoid,
        FunctionDeclarator(
          Identifier("print_int"),
          LParam(Vector(Cint -> Identifier("value")))
        )
      )
    )
  }
}