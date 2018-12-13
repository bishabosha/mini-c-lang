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
    val cursor = Cursor(context)
    val exitCode = new interpretAst(cursor, nodes).evalProgram
    println(s"exit code: $exitCode")
  }

  case class IdentKey(key: Variable) extends Bindings.Key {
    type Value = Constant
  }
}

class interpretAst private (var cursor: Cursor, nodes: Goal) {
  val random: Random = new Random
  val topLevel: Bindings = cursor.current

  private def evalProgram: Int = {
    println("interpreting:")
    nodes.foldLeft(None: Option[Int]){ (code, statement) =>
      code.orElse(topLevelDeclaration(statement))
    } getOrElse {
      throw SemanticError("Program does not terminate")
    }
  }

  private def topLevelDeclaration(node: Declarations): Option[Int] = node match {
    case Function(Std.`mainIdentifier`, _, body) =>
      stacked {
        body.foldLeft(None: Option[Int]){ (code, statement) =>
          code.orElse(evalStatement(statement))
        }
      }
    case Declaration(_, _, id: Scoped) =>
      addRandom(id)
      None
    case Assignment(dest, value) =>
      if cursor.current.genSearch(IdentKey(dest)).isDefined then {
        addValue(dest, value)
        None
      } else {
        unexpected(dest)
      }
    case _ => None
  }

  private def stacked(f: => Option[Int]): Option[Int] = {
    cursor = cursor.next
    f
  }

  private def evalStatement(node: Statements): Option[Int] = {
    node match {
      case Declaration(_, _, id: Scoped) =>
        addRandom(id)
        None
      case Assignment(dest: Temporary, value) =>
        addValue(dest, value)
        None
      case Assignment(dest: Scoped, value) =>
        if cursor.current.genSearch(IdentKey(dest)).isDefined then {
          addValue(dest, value)
          None
        } else {
          unexpected(dest)
        }
      case Return(Nil) =>
        None
      case Return(v) =>
        v.map(expr).lastOption.getOrElse { None }
      case _ => None
    }
  }

  private def addValue(k: Variable, v: Assignments): Unit = {
    addConstant(k, evalAsConstant(expr(v)))
  }

  private def addRandom(k: Variable): Unit = {
     addConstant(k, Constant(random.nextInt))
  }

  private def addConstant(k: Variable, c: Constant): Unit = {
     cursor = cursor.withBindings(cursor.current + (IdentKey(k), c))
  }

  private def evalAsConstant(o: Option[Int]): Constant = {
    o.map(Constant).getOrElse {
      throw UnimplementedError("expression does not yield constant")
    }
  }

  private def expr(node: Assignments): Option[Int] = {
    node match {
      case _ : Application =>
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
      case i: Scoped =>
        cursor.current.genSearch(IdentKey(i)).map(_.value).orElse {
          unexpected(i)
        }
      case t: Temporary =>
        cursor.current.genSearch(IdentKey(t)).map(_.value)
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