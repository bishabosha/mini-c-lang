package mycc

import Ast._
import StorageTypes._
import Types._
import RelationalOperators._
import AdditiveOperators._
import EqualityOperators._
import MultiplicativeOperators._
import UnaryOperators._
import ArgList._
import flattenAst._
import exception._
import interpretAst._
import scala.util.Random

object normalToTac extends Stage {
  type Source   = flattenAst.Goal
  type Context  = flattenAst.Context
  type Goal     = List[Statements]

  def apply(context: Context, nodes: Source): (Context, Goal) =
    new normalToTac(Cursor(Nil, Map(), context), nodes).goal
}

class normalToTac private (var cursor: Cursor, nodes: Goal) {
  val topLevel: Bindings = cursor.current
  val main = Identifier("main")

  private def goal: (Context, Goal) = {
    topLevel.local(main) match {
      case Some(Declaration(auto, int, FunctionDeclarator(`main`, LVoid)))
        if topLevel.definition(main).isDefined =>
          val code = nodes.foldLeft(Nil: Goal){ (code, statement) =>
            topLevelStatement(statement)
              .map(_::code)
              .getOrElse {
                throw UnimplementedError("Program has unimplemented features")
              }
          }.reverse
          (cursor.current, code)
      case _ =>
        throw SemanticError("function definition for `int main(void)` not found.")
    }
  }

  private def topLevelStatement(node: Statements): Option[Statements] = node match {
    case Function(id, body) if id == main =>
      stacked {
        val validated = body.foldLeft(Nil: List[Statements]){ (code, statement) =>
         evalStatement(statement).map(_ :: code).getOrElse{code}
        }.reverse
        Some(Function(id, validated))
      }
    case d : Declaration =>
      Some(d)
    case Assignment(id, value) =>
      None
    case t @ Temporary(value) =>
      None
    case _ => None
  }

  private def stacked[O](f: => Option[O]): Option[O] = {
    cursor = cursor.next.getOrElse { throw new IllegalStateException("no child") }
    f
  }

  private def evalStatement(node: Statements): Option[Statements] = {
    node match {
      case d: Declaration =>
        Some(d)
      case t @ Temporary(inner) if isAtomic(inner) =>
        Some(t)
      case a @ Assignment(_, inner) if isAtomic(inner) =>
        Some(a)
      case r @ Return(inner :: Nil) if isAtomic(inner) =>
        Some(r)
      case _ => None
    }
  }

  private def isAtomic(node: Assignments): Boolean = {
    node match {
      case v @ (
        _: Equality
      | _: Relational
      | _: Additive
      | _: Multiplicative
      | _: Unary
      | _: Constant
      | _: StringLiteral
      | _: Identifier
      | _: Relational
      ) =>
        true
      case Temporary(t) if isAtomic(t) =>
        true
      case _ =>
        false
    }
  }
}