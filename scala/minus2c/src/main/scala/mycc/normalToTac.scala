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

  def apply(context: Context, nodes: Source): Goal =
    new normalToTac(Cursor(Nil, Map(), context), nodes).goal
}

class normalToTac private (var cursor: Cursor, nodes: Goal) {
  val topLevel: Bindings = cursor.current
  val main = Identifier("main")

  private def goal: Goal = {
    topLevel.local(main) match {
      case Some(Declaration(auto, int, FunctionDeclarator(`main`, LVoid)))
        if topLevel.definition(main).isDefined =>
          println("interpreting:")
          nodes.foldLeft(Nil: Goal){ (code, statement) =>
            topLevelStatement(statement)
              .map(_::code)
              .getOrElse {
                throw SemanticError("Program does not terminate")
              }
          }.reverse
      case _ =>
        throw SemanticError("function definition for `int main(void)` not found.")
    }
  }

  private def topLevelStatement(node: Statements): Option[Statements] = node match {
    case Function(id, body) if id == main =>
      stacked {
        body.foldLeft(None: Option[Statements]){ (code, statement) =>
          code.orElse(evalStatement(statement))
        }
      }
    case Declaration(_, _, id: Identifier) =>
      None
    case _ : Declaration =>
      None
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
      case Declaration(_, _, id: Identifier) =>
        None
      case _: Declaration =>
        None
      case Assignment(id, value) =>
        None
      case t @ Temporary(value) =>
        None
      case _: Function =>
        None
      case _: Block =>
        None
      case Return(Nil) =>
        None
      case Return(v) =>
        None
      case _ => None
    }
  }

  private def expr(node: Assignments): Option[Statements] = {
    node match {
      case Application(i, args) =>
        None
      case Equality(op, l, r) =>
        None
      case Relational(op, l, r) =>
        None
      case Additive(op, l, r) =>
        None
      case Multiplicative(op, l, r) =>
        None
      case Unary(op, v) =>
        None
      case Constant(v) =>
        None
      case StringLiteral(str) =>
        None
      case i: Identifier =>
        None
      case t: Temporary =>
        None
      case x =>
        throw UnexpectedAstNode(s"expression: ${x.toString}")
    }
  }
}