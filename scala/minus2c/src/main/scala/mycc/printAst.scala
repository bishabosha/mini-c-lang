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
import exception._

object printAst {

  private val endl = System.lineSeparator

  def apply(context: Bindings, nodes: List[Statements]): Unit =
    print(astNode(context, nodes, 0))

  private def astNode
    ( context: Bindings,
      nodes: List[Statements],
      level: Int
    ): String = {
      (for (node <- nodes.view) yield astNode(context, node, level)).mkString
    }

  private def astNode
    ( context: Bindings,
      node: Statements,
      level: Int
    ): String = {
      def getIt(value: Statements): String = astNode(context, value, level);
      def exp(value: Assignments): String = expr(context, value, level);
      var lvl = getLevel(level)
      node match {
        case Declaration(storage, types, declarator) =>
          declarator match {
            case Identifier(id) =>
              s"$lvl${storageToString(storage)}${typesToString(types)} $id;$endl"
            case FunctionDeclarator(Identifier(id), args) =>
              s"$lvl${storageToString(storage)}${typesToString(types)} $id${getArgList(args)};$endl"
          }
        case Assignment(Identifier(id), value) =>
          s"$lvl$id = ${exp(value)};$endl"
        case t @ Temporary(v) =>
          s"$lvl${getId(t)} = ${exp(v)};$endl"
        case Function(i @ Identifier(id), b) =>
          fn(context, id, b, level)
        case Block(b) => block(context, b, level)
        case Return(Nil) => s"${lvl}return;$endl"
        case Return(v) =>
          val newArgs = v.map(exp).mkString(", ")
          s"${lvl}return $newArgs;$endl"
        case _ => s"${lvl}???$endl"
      }
    }

  private def expr
    ( context: Bindings,
      node: Assignments,
      level: Int
    ): String = {
      def getIt(value: Assignments): String = expr(context, value, level)
      node match {
        case Application(i, args) =>
          val newArgs = args.map(getIt).mkString(", ")
          s"${getIt(i)}($newArgs)"
        case Equality(op, l, r) =>
          getBinary(op, getIt(l), getIt(r))
        case Relational(op, l, r) =>
          getBinary(op, getIt(l), getIt(r))
        case Additive(op, l, r) =>
          getBinary(op, getIt(l), getIt(r))
        case Multiplicative(op, l, r) =>
          getBinary(op, getIt(l), getIt(r))
        case Unary(op, v) =>
          getUnary(op, getIt(v))
        case Constant(v) =>
          s"$v"
        case StringLiteral(str) =>
          s""""$str""""
        case Identifier(i) =>
          s"$i"
        case t: Temporary =>
          getId(t)
        case x =>
          throw UnexpectedAstNode(s"expression: ${x.toString}")
      }
    }

  private def getId(t: Temporary): String = ("_" + t.hashCode).take(6)

  private def getUnary(op: Operand, v: String): String =
    s"${op.symbol} $v"

  private def getBinary(op: Operand, l: String, r: String): String =
    s"$l ${op.symbol} $r"

  private def getArgList(a: ArgList): String = a match {
    case LVoid => "(void)"
    case LAny => "()"
    case LParam(params) =>
      params.view.map {
        case t: Types => s"$t"
        case (t: Types, _ @ Identifier(i)) => s"$t $i"
      }.mkString("(", ", ", ")")
  }

  private def fn
    ( context: Bindings,
      name: String,
      b: List[Statements],
      level: Int
    ): String = {
      var lvl: String = getLevel(level)
      var res = s"$lvl$name: {$endl"
      if !b.isEmpty then
        res + astNode(context, b, inc(level)) + s"$lvl}$endl"
      else
        res + s"}$endl"
    }

  private def block
    ( context: Bindings,
      b: List[Statements],
      level: Int
    ): String = {
      var lvl: String = getLevel(level)
      if b.isEmpty then
        s"$lvl{}$endl"
      else
        s"$lvl{$endl${astNode(context, b, inc(level))}$lvl}$endl"
    }

  private def inc(level: Int) = level + 2

  private def storageToString(s: StorageTypes) = s match {
    case Auto => ""
    case _ => s"$s "
  }

  private def typesToString(s: Types) = s match {
    case Cint => "int"
    case Cvoid => "void"
    case Cfunction => "function"
  }

  private def getLevel(l: Int): String = " " * l

  private def getRoot(temporary: Temporary): Assignments =
    temporary.rvalue match {
      case t: Temporary => getRoot(t)
      case x => x 
    }
}