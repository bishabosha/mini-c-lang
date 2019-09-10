package mmc

import Ast._
import Constants._
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
  type Value = Declaration
}

case class DefinitionKey(id: Scoped) extends Bindings.Key {
  type Value = Unit
}

val zero = IntLiteral(0)

def replaceHead[A](list: List[A])(f: A => A): List[A] =
  f(list.head) :: list.tail

def mainDefined[A](topLevel: Bindings)(f: Bindings => A): A =
  topLevel.genGet(Std.mainIdentifierKey) match {
    case Some(Std.`mainFunc`) =>
      if topLevel.genGet(Std.mainDefinitionKey) isDefined then
        f(topLevel)
      else {
        throw SemanticError(
          "function definition for `auto int main(void)` not found.")
      }
    case _ =>
      throw SemanticError(
        "function declaration for `auto int main(void)` not found.")
  }

def extractDeclarations
  (declarations: List[Declaration]): Map[Bindings.Key, Any] =
    (for (d @ Declaration(_, _, decl) <- declarations)
      yield (DeclarationKey(extractIdentifier(decl)), d)
    ).toMap

private def extractIdentifier(d: Declarator): Identifier = d match {
  case Scoped(id, _) => id
  case _ @ FunctionDeclarator(Scoped(id, _), _) => id
}

def printScopesOrdered(bindings: Bindings): Unit = {
  var cursor = Cursor(bindings)
  while (!cursor.isEmpty) {
    val scope = getCurrentScope(cursor.current)
    println(scope)
    cursor = cursor.next
  }
}

def unexpected(lvalue: Variable): Nothing = {
  throw SemanticError(s"unknown variable ${showVariable(lvalue)}")
}

def showVariable(lvalue: Variable): String =
  lvalue match {
    case Scoped(id,s) => s"$id~$s"
    case t: Temporary => showTemporary(t)
  }

def showTemporary(temporary: Temporary): String =
  ("@" + temporary.hashCode).take(6)

def getCurrentScope(bindings: Bindings): Long =
  bindings.genGet(ScopeKey).getOrElse {
    throw new IllegalStateException("Context has no scope!")
  }

object Std {
  val mainIdentifier = Scoped(Identifier("main"),0)
  val mainIdentifierKey = DeclarationKey(Std.mainIdentifier.id)
  val mainDefinitionKey = DefinitionKey(Std.mainIdentifier)
  val mainFunc =
    Declaration(
      Auto,
      Cint,
      FunctionDeclarator(mainIdentifier, LVoid)
    )
  val printIntIdentifier = Scoped(Identifier("print_int"),0)
  val readIntIdentifier = Scoped(Identifier("read_int"),0)
  val print_int =
    Declaration(
      Extern,
      Cvoid,
      FunctionDeclarator(
        printIntIdentifier,
        LParam(Vector(Cint -> Scoped(Identifier("value"),0)))
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
  val declarations = List[Declaration](
    print_int,
    read_int
  )
}
