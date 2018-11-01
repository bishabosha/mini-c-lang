package mycc

import Ast._
import ArgList._
import astToNormal._
import exception._

object normalToTac extends Stage {
  type Source   = astToNormal.Goal
  type Context  = astToNormal.Context
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
            topLevelStatement(statement) ++ code
          }.reverse
          (topLevel, code) // TODO: replace topLevel with context.current once stacked pops off frame
      case _ =>
        throw SemanticError("function definition for `int main(void)` not found.")
    }
  }

  private def topLevelStatement(node: Statements): Goal = node match {
    case Function(id, body) if id == main =>
      stacked {
        val validated = body.foldLeft(Nil: List[Statements]){ (code, statement) =>
         evalStatement(statement) ++ code
        }.reverse
        List(Function(id, validated))
      }
    case _ => Nil
  }

  private def stacked[O](f: => List[O]): List[O] = {
    cursor = cursor.next.getOrElse { throw new IllegalStateException("no child") }
    f
  }

  private def evalStatement(node: Statements): Goal = {
    node match {
      case d: Declaration =>
        List(d)
      case t @ Temporary(inner) if isAtomic(inner) =>
        List(t)
      case a @ Assignment(_, inner) if isAtomic(inner) =>
        List(a)
      case r @ Return(inner :: Nil) if isAtomic(inner) =>
        List(r)
      case _ =>
        List()
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