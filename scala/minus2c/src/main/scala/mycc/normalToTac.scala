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