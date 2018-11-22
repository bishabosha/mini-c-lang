package mycc

import Ast._
import ArgList._
import astToNormal._
import exception._
import StorageTypes._
import Types._

object normalToTac extends Stage {
  type Source   = astToNormal.Goal
  type Context  = astToNormal.Context
  type Goal     = List[Statements]

  def apply(context: Context, nodes: Source): (Context, Goal) =
    new normalToTac(Cursor.Empty.copy(current = context), nodes).goal
}

class normalToTac private (var cursor: Cursor, nodes: Goal) {
  val topLevel: Bindings = cursor.current

  private def goal: (Context, Goal) = {
    topLevel.genGet(Std.mainIdentifierKey) match {
      case Some(Std.`mainFunc`)
        if topLevel.genGet(Std.mainDefinitionKey).isDefined =>
          val code = nodes.foldLeft(Nil: Goal){ (code, statement) =>
            topLevelStatement(statement) ++ code
          }.reverse
          (topLevel, code)
          // TODO: replace topLevel with context.current once stacked pops off
          // frame
      case _ =>
        throw SemanticError(
          "function definition for `int main(void)` not found.")
    }
  }

  private def topLevelStatement(node: Statements): Goal = node match {
    case Function(Std.`mainIdentifier`, body) =>
      stacked {
        val validated = body
          .foldLeft(Nil: List[Statements]){ (code, statement) =>
            evalStatement(statement) ++ code
          }.reverse
        List(Function(Std.mainIdentifier, validated))
      }
    case a: Assignment => List(a)
    case d: Declaration => List(d)
    case _ => Nil
  }

  private def stacked[O](f: => List[O]): List[O] = {
    cursor = cursor.next.getOrElse {
      throw new IllegalStateException("no child")
    }
    f
  }

  private def evalStatement(node: Statements): Goal = {
    node match {
      case d: Declaration =>
        List(d)
      case t @ Temporary(Expression()) =>
        List(t)
      case a @ Assignment(_, Expression()) =>
        List(a)
      case r @ Return(Expression() :: Nil) =>
        List(r)
      case _ =>
        List()
    }
  }
}