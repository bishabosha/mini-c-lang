package mycc

import Ast._
import ArgList._
import astToNormal._
import normalToTacActual._
import exception._
import StorageTypes._
import Types._
import Tac._

object normalToTacActual extends Stage {
  type Source   = astToNormal.Goal
  type Context  = astToNormal.Context
  type Goal     = (Map[Identifier, GlobalData], List[Tac])

  def apply(context: Context, nodes: normalToTacActual.Source): (Context, Goal) =
    new normalToTacActual(Cursor(context), nodes).goal
}

class normalToTacActual private (var cursor: Cursor, nodes: normalToTacActual.Source) {
  val topLevel: Bindings = cursor.current

  private def goal: (Context, normalToTacActual.Goal) = {
    topLevel.genGet(Std.mainIdentifierKey) match {
      case Some((Std.`mainFunc`, 0))
        if topLevel.genGet(Std.mainDefinitionKey).isDefined =>
          val code = nodes.foldLeft(Nil: List[Tac]){ (code, statement) =>
            topLevelStatement(statement) ++ code
          }.reverse
          (topLevel, (Map(), code))
          // TODO: replace topLevel with context.current once stacked pops off
          // frame
      case _ =>
        throw SemanticError(
          "function declaration for `int main(void)` not found.")
    }
  }

  private def getData(node: Statements): Option[(Identifier, GlobalData)] = node match {
    case Declaration(_, _, i: Identifier) =>
      Some((i, GlobalWord(i, zero)))
    case Assignment(i: Identifier, c: Constant) =>
      Some((i, GlobalWord(i, c)))
    case _ => None
  }

  private def topLevelStatement(node: Statements): List[Tac] = node match {
    case Function(Std.`mainIdentifier`, f, body) =>
      stacked {
        Nil
      }
    case _ => Nil
  }

  private def stacked[O](f: => List[O]): List[O] = {
    cursor = cursor.next
    f
  }

  import AssignmentsPattern._
  private def evalStatement(node: Statements): List[Tac] = {
    Nil
  }
}