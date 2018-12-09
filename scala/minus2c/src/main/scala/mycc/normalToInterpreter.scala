package mycc

import Ast._
import ArgList._
import astToNormal._
import exception._
import StorageTypes._
import Types._

object normalToInterpreter extends Stage {
  type Source   = astToNormal.Goal
  type Context  = astToNormal.Context
  type Goal     = List[Statements]

  def apply(context: Context, nodes: Source): (Context, Goal) =
    new normalToInterpreter(Cursor(context), nodes).goal
}

class normalToInterpreter private (var cursor: Cursor, nodes: Goal) {
  val topLevel: Bindings = cursor.current

  private def goal: (Context, Goal) = parseMain(topLevel) { () =>
    val code = nodes.foldLeft(Nil: Goal){ (code, statement) =>
      topLevelStatement(statement) ++ code
    }.reverse
    (topLevel, code)
    // TODO: replace topLevel with context.current once stacked pops off
    // frame
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
      case a @ Assignment(_, _: ExpressionRoot) =>
        List(a)
      case r @ Return((_: ExpressionRoot) :: Nil) =>
        List(r)
      case _ =>
        List()
    }
  }
}