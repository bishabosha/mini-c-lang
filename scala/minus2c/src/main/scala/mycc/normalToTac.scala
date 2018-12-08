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
    new normalToTac(Cursor(context), nodes).goal
}

class normalToTac private (var cursor: Cursor, nodes: Goal) {
  val topLevel: Bindings = cursor.current

  private def goal: (Context, Goal) = {
    topLevel.genGet(Std.mainIdentifierKey) match {
      case Some((Std.`mainFunc`, 0))
        if topLevel.genGet(Std.mainDefinitionKey).isDefined =>
          val code = nodes.foldLeft(Nil: Goal){ (code, statement) =>
            topLevelStatement(statement) ++ code
          }.reverse
          (topLevel, code)
          // TODO: replace topLevel with context.current once stacked pops off
          // frame
      case _ =>
        throw SemanticError(
          "function declaration for `int main(void)` not found.")
    }
  }

  private def topLevelStatement(node: Statements): Goal = node match {
    case Function(Std.`mainIdentifier`, f, body) =>
      stacked {
        val validated = body
          .foldLeft(Nil: List[Statements]){ (code, statement) =>
            evalStatement(statement) ++ code
          }.reverse
        List(Function(Std.mainIdentifier, f, validated))
      }
    case a: Assignment => List(a)
    case d: Declaration => List(d)
    case _ => Nil
  }

  private def stacked[O](f: => List[O]): List[O] = {
    cursor = cursor.next
    f
  }

  private def evalStatement(node: Statements): Goal = {
    node match {
      case d @ Declaration(_, _, _: Identifier) =>
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