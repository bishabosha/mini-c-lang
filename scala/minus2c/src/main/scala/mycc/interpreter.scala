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
import astToNormal._
import exception._
import interpretAst._
import scala.util.Random

object interpretAst {

  def apply(context: Context, nodes: Goal): Unit = {
    println(s"exit code: ${new interpretAst(Cursor(Nil, Map(), context), nodes).evalProgram}")
  }
}

class interpretAst private (var cursor: Cursor, nodes: Goal) {
  val random: Random = new Random
  val topLevel: Bindings = cursor.current
  val main = Identifier("main")

  private def evalProgram: Int = {
    topLevel.local(main) match {
      case Some(Declaration(auto, int, FunctionDeclarator(`main`, LVoid)))
        if topLevel.definition(main).isDefined =>
          println("interpreting:")
          nodes.foldLeft(None: Option[Int]){ (code, statement) =>
            code.orElse(topLevelStatement(statement))
          } getOrElse {
            throw SemanticError("Program does not terminate")
          }
      case _ =>
        throw SemanticError("function definition for `int main(void)` not found.")
    }
  }

  private def topLevelStatement(node: Statements): Option[Int] = node match {
    case Function(id, body) if id == main =>
      stacked {
        body.foldLeft(None: Option[Int]){ (code, statement) =>
          code.orElse(evalStatement(statement))
        }
      }
    case Declaration(_, _, id: Identifier) =>
      addRandom(id)
      None
    case _ : Declaration =>
      None
    case Assignment(id, value) =>
      addValue(id, value)
      None
    case t @ Temporary(value) =>
      addValue(t, value)
      None
    case _ => None
  }

  private def stacked(f: => Option[Int]): Option[Int] = {
    cursor = cursor.next.getOrElse {
      throw new IllegalStateException("no child")
    }
    f
  }

  private def evalStatement(node: Statements): Option[Int] = {
    node match {
      case Declaration(_, _, id: Identifier) =>
        addRandom(id)
        None
      case _: Declaration =>
        None
      case Assignment(id, value) =>
        addValue(id, value)
        None
      case t @ Temporary(value) =>
        addValue(t, value)
        None
      case _: Function =>
        None
      case _: Block =>
        None
      case Return(Nil) =>
        None
      case Return(v) =>
        v.map(expr).lastOption.getOrElse { None }
      case _ => None
    }
  }

  private def addValue(k: Key | Temporary, v: Assignments): Unit = {
    addConstant(k, evalAsConstant(expr(v)))
  }

  private def addRandom(k: Key | Temporary): Unit = {
     addConstant(k, Constant(random.nextInt))
  }

  private def addConstant(k: Key | Temporary, c: Constant): Unit = {
     cursor += (k -> c)
  }

  private def evalAsConstant(o: Option[Int]): Constant = {
    o.map(Constant).getOrElse {
      throw UnimplementedError("expression does not yield constant")
    }
  }

  private def expr(node: Assignments): Option[Int] = {
    node match {
      case Application(i, args) =>
        None
      case Equality(op, l, r) =>
        binary(op, l, r)
      case Relational(op, l, r) =>
        binary(op, l, r)
      case Additive(op, l, r) =>
        binary(op, l, r)
      case Multiplicative(op, l, r) =>
        binary(op, l, r)
      case Unary(op, v) =>
        unary(op, v)
      case Constant(v) =>
        Some(v)
      case StringLiteral(str) =>
        None
      case i: Identifier =>
        cursor.value(i)
      case t: Temporary =>
        cursor.value(t)
      case x =>
        throw UnexpectedAstNode(s"expression: ${x.toString}")
    }
  }

  def unary(op: UnaryOp, v: Assignments): Option[Int] =
    for {
      uv <- expr(v)
    } yield op.op(uv)

  def binary(op: BinaryOp, l: Assignments, r: Assignments): Option[Int] =
    for {
      lv <- expr(l)
      rv <- expr(r)
    } yield op.op(lv, rv)
}