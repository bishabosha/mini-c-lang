package mycc

import Ast._
import ArgList._
import exception._
import tacToMips._
import MIPS._

object tacToMips extends Stage {
  type Source   = normalToTac.Goal
  type Context  = normalToTac.Context
  type Goal     = List[Assembler]

  def apply(context: Context, nodes: Source): (Context, Goal) =
    new tacToMips(Cursor(Nil, Map(), context), nodes).goal
}

class tacToMips private (var cursor: Cursor, nodes: normalToTac.Goal) {
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
          (topLevel, code) // TODO: replace topLevel with context.current once stacked pops off frame
      case _ =>
        throw SemanticError("function definition for `int main(void)` not found.")
    }
  }

  private def topLevelStatement(node: Statements): Option[Assembler] = node match {
    case Function(id, body) if id == main =>
      None
    case d : Declaration =>
      None
    case _ => None
  }

  private def stacked[O](f: => Option[O]): Option[O] = {
    cursor = cursor.next.getOrElse { throw new IllegalStateException("no child") }
    f
  }

  private def evalStatement(node: Statements): Option[Assembler] = {
    node match {
      case d: Declaration =>
        None
      case t @ Temporary(inner) if isAtomic(inner) =>
        None
      case a @ Assignment(_, inner) if isAtomic(inner) =>
        None
      case r @ Return(inner :: Nil) if isAtomic(inner) =>
        None
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