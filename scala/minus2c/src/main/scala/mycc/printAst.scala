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

  def apply(nodes: List[Declarations]): Unit =
    print(astNode(nodes, 0))

  private def astNode
    ( nodes: List[Statements],
      level: Int
    ): String = {
      (for (node <- nodes.view) yield astNode(node, level)).mkString
    }

  private def astNode
    ( node: Statements,
      level: Int
    ): String = {
      def getIt(value: Statements): String = astNode(value, level);
      def exp(value: Assignments): String = expr(value, level);
      var lvl = getLevel(level)
      node match {
        case Declaration(storage, types, declarator) =>
          declarator match {
            case Scoped(Identifier(id),_) =>
              s"$lvl${storageToString(storage)}${typesToString(types)} $id;$endl"
            case FunctionDeclarator(Scoped(Identifier(id),_), args) =>
              s"$lvl${storageToString(storage)}${typesToString(types)} $id${getArgList(args)};$endl"
          }
        case Assignment(Scoped(Identifier(id),_), value) =>
          s"$lvl$id = ${exp(value)};$endl"
        case Assignment(t: Temporary, value) =>
          s"$lvl${showTemporary(t)} = ${exp(value)};$endl"
        case Function(i @ Scoped(Identifier(id),_), _, b) =>
          fn(id, b, level)
        case Block(b) => block(b, level)
        case Return(Nil) => s"${lvl}return;$endl"
        case Return(v) =>
          val newArgs = v.map(exp).mkString(", ")
          s"${lvl}return $newArgs;$endl"
        case IfElse(_, test, ifThen, orElse) =>
          ifElse(test,ifThen,orElse,level)
        case _ => s"${lvl}???$endl"
      }
    }

  private def expr
    ( node: Assignments,
      level: Int
    ): String = {
      def getIt(value: Assignments): String = expr(value, level)
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
        case Scoped(Identifier(i),_) =>
          s"$i"
        case t: Temporary =>
          showTemporary(t)
        case x =>
          throw UnexpectedAstNode(s"expression: ${x.toString}")
      }
    }

  private def getUnary(op: Operand, v: String): String =
    s"${op.symbol} $v"

  private def getBinary(op: Operand, l: String, r: String): String =
    s"$l ${op.symbol} $r"

  private def getArgList(a: ArgList): String = a match {
    case LVoid => "(void)"
    case LAny => "()"
    case LParam(params) =>
      params.view.map {
        case t: Types => typesToString(t)
        case (t: Types, Scoped(Identifier(i),_)) => s"${typesToString(t)} $i"
      }.mkString("(", ", ", ")")
  }

  private def fn
    ( name: String,
      b: List[Statements],
      level: Int
    ): String = {
      var lvl: String = getLevel(level)
      var res = s"$lvl$name: {$endl"
      if !b.isEmpty then
        res + astNode(b, inc(level)) + s"$lvl}$endl"
      else
        res + s"}$endl"
    }

  private def block
    ( b: List[Statements],
      level: Int
    ): String = {
      var lvl: String = getLevel(level)
      if b.isEmpty then
        s"$lvl{}$endl"
      else
        s"$lvl{$endl${astNode(b, inc(level))}$lvl}$endl"
    }

  private def ifElse
    ( test: Expressions,
      ifThen: List[Statements],
      orElse: Option[List[Statements]],
      level: Int
    ): String = {
      var lvl: String = getLevel(level)
      val testStr = test.map(expr(_,level)).mkString(", ")
      val ifThenStr = astNode(ifThen, inc(level))
      if orElse.isEmpty then
        s"${lvl}if ($testStr) {$endl$ifThenStr$lvl}$endl"
      else {
        val orElseG = orElse.get
        if orElseG.isEmpty then {
          s"${lvl}if ($testStr) {$endl$ifThenStr$lvl} else {$endl$lvl}$endl"
        } else {
          val orElseStr = astNode(orElseG, inc(level))
          s"${lvl}if ($testStr) {$endl$ifThenStr$lvl} else {$endl$orElseStr$lvl}$endl"
        }
      }

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
    case Cstring => "string"
  }

  private def getLevel(l: Int): String = " " * l
}