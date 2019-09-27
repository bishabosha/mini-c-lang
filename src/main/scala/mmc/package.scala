package mmc

import Constants._
import exception._
import ArgList._

given Conversion[Boolean, Int] = if _ then 1 else 0

inline def const[A,B](a: A) = (b: B) => a

inline def True[A]: A => Boolean = const(true)

type =?>[I,O] = PartialFunction[I,O]

case object ScopeKey extends Bindings.Key
  type Value = Long

case class DeclarationKey(id: Identifier) extends Bindings.Key
  type Value = Declaration

case class DefinitionKey(id: Scoped) extends Bindings.Key
  type Value = Unit

val zero = IntLiteral(0)

def replaceHead[A](list: List[A])(f: A => A): List[A] =
  f(list.head) :: list.tail

def mainDefined[A](topLevel: Bindings)(f: Bindings => A): A =
  topLevel.genGet(Std.mainIdentifierKey) match
    case Some(Std.`mainFunc`) =>
      if topLevel.genGet(Std.mainDefinitionKey) isDefined then
        f(topLevel)
      else
        throw SemanticError(
          "function definition for `auto int main(void)` not found.")
    case _ =>
      throw SemanticError(
        "function declaration for `auto int main(void)` not found.")

def extractDeclarations(declarations: Declaration*): Seq[(DeclarationKey, DeclarationKey#Value)] =
  for d @ Declaration(_, _, decl) <- declarations yield
    DeclarationKey(extractIdentifier(decl)) -> d

private def extractIdentifier(d: Declarator): Identifier = d match
  case Scoped(id, _)                        => id
  case FunctionDeclarator(Scoped(id, _), _) => id

def printScopesOrdered(bindings: Bindings): Unit =
  var cursor = Cursor(bindings)
  while !cursor.isEmpty do
    val scope = getCurrentScope(cursor.current)
    println(scope)
    cursor = cursor.next

def unexpected(lvalue: Variable): Nothing =
  throw SemanticError(s"unknown variable ${showVariable(lvalue)}")

def showVariable(lvalue: Variable): String = lvalue match
  case Scoped(id,s) => s"$id~$s"
  case t: Temporary => showTemporary(t)

def showTemporary(temporary: Temporary): String =
  ("@" + temporary.hashCode).take(6)

def getCurrentScope(bindings: Bindings): Long =
  bindings.genGet(ScopeKey).getOrElse {
    throw IllegalStateException("Context has no scope!")
  }
