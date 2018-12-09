package object mycc {
  import Ast._
  import StorageTypes._
  import Types._
  import parseCAst._
  import exception.SemanticError
  import exception.UnexpectedAstNode
  import ArgList._
  import MIPS._
  import Tac._

  import scala.language.implicitConversions
  implicit def bool2Int(b: Boolean): Int = if b then 1 else 0

  case object ScopeKey extends Bindings.Key {
    type Value = Long
  }

  case class DeclarationKey(id: Identifier) extends Bindings.Key {
    type Value = (Declaration, Long)
  }

  case class DefinitionKey(id: Identifier) extends Bindings.Key {
    type Value = Unit
  }

  val zero = Constant(0)

  type LValue = Identifier | Temporary

  def replaceHead[A](list: List[A])(f: A => A): List[A] =
    f(list.head) :: list.tail

  def parseMain[A](topLevel: Bindings)(f: () => A): A =
    topLevel.genGet(Std.mainIdentifierKey) match {
        case Some((Std.`mainFunc`, 0)) =>
          if topLevel.genGet(Std.mainDefinitionKey) isDefined then
            f()
          else {
            throw SemanticError(
              "function definition for `int main(void)` not found.")
          }
        case _ =>
          throw SemanticError(
            "function declaration for `int main(void)` not found.")
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

  def renameMainFunc
    ( scope: Long,
      bindings: Bindings,
      id: Identifier,
      tac: List[Tac]
    ): (Bindings, List[Tac]) = {
      val newMainDecl = replaceIdent(Std.mainFunc, id)
      val newBindings = rename(
        scope,
        Std.mainIdentifier,
        id,
        newMainDecl,
        bindings
      )
      val newCode =
        renameFunc(tac)(id,Std.mainIdentifier)
      (newBindings,newCode)
    }

  def renameFunc
    ( tac: List[Tac] )
    ( id: Identifier,
      old: Identifier,
    ): List[Tac] = tac.foldRight(Nil: List[Tac]) { (s,acc) =>
      s match {
        case Func(`old`, f, body) =>
          Func(id, f, body) :: acc
        case any => any :: acc
      }
    }

  def rename
    ( scope: Long,
      old: Identifier,
      id: Identifier,
      decl: Declaration,
      bindings: Bindings
    ): Bindings =
      declareIn(id, decl, scope) {
        defineIn(id) {
          undefineIn(old) {
            undeclareIn(old) {
              bindings
            }
          }
        }
      }

  def unexpected(lvalue: LValue): Nothing = {
    throw UnexpectedAstNode(s"unknown variable ${showLValue(lvalue)}")
  }
  
  def showLValue(lvalue: LValue): String =
    lvalue match {
      case Identifier(id) => id
      case t: Temporary => showTemporary(t)
    }

  def showTemporary(temporary: Temporary): String =
    ("@" + temporary.hashCode).take(6)

  private def defineIn
    (key: Identifier)
    (bindings: => Bindings): Bindings =
      bindings + (DefinitionKey(key), ())

  private def undefineIn
    (key: Identifier)
    (bindings: => Bindings): Bindings =
      bindings - DefinitionKey(key)

  private def declareIn
    (key: Identifier, value: Declaration, scope: Long)
    (bindings: => Bindings): Bindings =
      bindings + (DeclarationKey(key), value -> scope)

  private def undeclareIn
    (key: Identifier)
    (bindings: => Bindings): Bindings =
      bindings - DeclarationKey(key)

  def getCurrentScope(bindings: Bindings): Long =
    bindings.genGet(ScopeKey).getOrElse {
      throw new IllegalStateException("Context has no scope!")
    }

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