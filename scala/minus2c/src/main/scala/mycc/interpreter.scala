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

object interpretAst {
  case class Cursor(stack: List[Cursor], values: Map[Key | Temporary, Constant], current: Bindings) {
    def +(pair: (Key | Temporary, Constant)): Cursor = Cursor(stack, values + pair, current)

    def value(key: Key | Temporary): Option[Int] =
      (this :: stack)
        .view
        .map(_.values.get(key))
        .collectFirst { case Some(o) => o.value }

    def next: Option[Cursor] = current.children.headOption.map { Cursor(this :: stack, Map(), _) }
  }

  def apply(context: Context, nodes: Goal): Unit = {
    println(s"exit code: ${new interpretAst(Cursor(Nil, Map(), context), nodes).evalProgram}")
  }
}

class interpretAst(var cursor: Cursor, nodes: Goal) {
  val random: Random = new Random
  val topLevel: Bindings = cursor.current
  val main = Identifier("main")

  private def evalProgram: Int = {
    cursor.current.local(main) match {
      case Some(Declaration(auto, int, FunctionDeclarator(`main`, LVoid))) if cursor.current.definition(main).isDefined =>
        println("RUNNING:")
        nodes.foldLeft(None: Option[Int]){ (code, statement) =>
          code.orElse(topLevelStatement(statement))
        } getOrElse {
          throw SemanticError("Program does not terminate")
        }
      case _ =>
        throw SemanticError("function definition for `int main(void)` not found.")
    }
  }

  private def topLevelStatement(node: Ast): Option[Int] = node match {
    case Function(id, body) if id == main =>
      stacked {
        body.foldLeft(None: Option[Int]){ (code, statement) =>
          code.orElse(evalStatement(statement))
        }
      }
    case Declaration(_, _, id: Identifier) =>
      cursor += (id -> Constant(random.nextInt))
      None
    case _ : Declaration =>
      None
    case Assignment(id, value) =>
      cursor += (id -> evalToConstant(expr(value)))
      None
    case t @ Temporary(value) =>
      cursor += (t -> evalToConstant(expr(value)))
      None
    case _ => None
  }

  private def stacked(f: => Option[Int]): Option[Int] = {
    cursor = cursor.next.getOrElse { throw new IllegalStateException("no child") }
    f
  }

  private def evalStatement(node: Ast): Option[Int] = {
    node match {
      case Declaration(_, _, id: Identifier) =>
        cursor += (id -> Constant(random.nextInt))
        None
      case _: Declaration =>
        None
      case Assignment(id, value) =>
        cursor += (id -> evalToConstant(expr(value)))
        None
      case t @ Temporary(value) =>
        cursor += (t -> evalToConstant(expr(value)))
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

  private def evalToConstant(o: Option[Int]): Constant = {
    o.map{Constant}.getOrElse{throw UnimplementedError("expression does not yield constant")}
  }

  private def expr(node: Assignments): Option[Int] = {
    node match {
      case Application(i, args) =>
        None
      case Equality(op, l, r) =>
        for {
          lv <- expr(l)
          rv <- expr(r)
        } yield op.op(lv, rv)
      case Relational(op, l, r) =>
        for {
          lv <- expr(l)
          rv <- expr(r)
        } yield op.op(lv, rv)
      case Additive(op, l, r) =>
        for {
          lv <- expr(l)
          rv <- expr(r)
        } yield op.op(lv, rv)
      case Multiplicative(op, l, r) =>
        for {
          lv <- expr(l)
          rv <- expr(r)
        } yield op.op(lv, rv)
      case Unary(op, v) =>
        for {
          uv <- expr(v)
        } yield op.op(uv)
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

  // private def getId(t: Temporary): String = s"_${t.hashCode}".take(6)

  // private def getUnary(op: Operand, v: String): String =
  //   s"${op.symbol} $v"

  // private def getBinary(op: Operand, l: String, r: String): String =
  //   s"$l ${op.symbol} $r"

  // private def getArgList(a: ArgList): String = a match {
  //   case LVoid => "(void)"
  //   case LAny => "()"
  //   case LParam(params) =>
  //     params.view.map {
  //       case t: Types => s"$t"
  //       case (t: Types, _ @ Identifier(i)) => s"$t $i"
  //     }.mkString("(", ", ", ")")
  // }

  // private def fn(context: Context, name: String, b: List[Statements], level: Int): String = {
  //   var lvl: String = getLevel(level)
  //   var res = s"$lvl$name: {$endl"
  //   if (!b.isEmpty)
  //     res + astNode(context, b, inc(level)) + s"$lvl}$endl"
  //   else
  //     res + s"}$endl"
  // }

  // private def block(context: Context, b: List[Statements], level: Int): String = {
  //   var lvl: String = getLevel(level)
  //   if (b.isEmpty) {
  //     s"$lvl{}$endl"
  //   } else {
  //     s"$lvl{$endl${astNode(context, b, inc(level))}$lvl}$endl"      
  //   }
  // }

  // private def inc(level: Int) = level + 2

  // private def storageToString(s: StorageTypes) = s match {
  //   case StorageTypes.auto => ""
  //   case _ => s"$s "
  // }

  // private def getLevel(l: Int): String = " " * l

  // private def getRoot(temporary: Temporary): Assignments = temporary.rvalue match {
  //   case t: Temporary => getRoot(t)
  //   case x => x 
  // }
}