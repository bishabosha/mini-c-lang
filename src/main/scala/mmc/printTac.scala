package mmc

import Tac._
import MiscTwoOperators._
import MiscOneOperators._
import TwoControlOperators._
import OneControlOperators._
import exception._
import Constants._
import Types._

object printTac

  private val endl = System.lineSeparator

  def apply(context: normalToTac.DataMap, src: List[Tac]): Unit =
    print(tac(context, src, "  "))

  private def tac(data: normalToTac.DataMap, src: List[Tac], indent: String): String =
    val dataString = s"$indent.static$endl"
    val codeString = s"$indent.code$endl"
    val dataAll = evalData(data, indent)
    val codeAll = evalNodes(src, indent)
    dataString + dataAll + codeString + codeAll

  private def evalData(data: normalToTac.DataMap, indent: String): String =
    val entries = data.map:
      case Scoped(i,s) -> c => s"$i~$s:$endl${indent}.const %I32 $c$endl"
    entries.mkString

  private def evalNodes(nodes: List[Tac], indent: String): String =
    nodes.map(evalNode(_, indent)).mkString

  private def evalNode(node: Tac, indent: String): String =
    val Func(scoped, frame, codes) = node
    evalFunction(scoped, frame, codes, indent)

  private def evalFunction(scoped: Scoped, frame: Frame, codes: List[Code], indent: String ): String =
    s"${scoped.id}~${scoped.scope}:$endl" +
    s"$indent.begin_function$endl" +
    evalFrame(frame, indent) +
    evalCodes(codes, indent) +
    s"$indent.end_function$endl"

  private def evalFrame(frame: Frame, indent: String): String =
    val globals = frame.globals.map:
      case a -> Declaration(_,t,_) => s"${indent}.global ${evalType(t)} $a~0$endl"
    val locals = frame.locals.map:
      case Scoped(a, scope) -> Declaration(_,t,_) => s"${indent}.local ${evalType(t)} $a~$scope$endl"
    val params = frame.params.map:
      case Scoped(a, scope) -> Declaration(_,t,_) => s"${indent}.param ${evalType(t)} $a~$scope$endl"
    val captured = frame.captures.map:
      case Scoped(a, scope) -> Declaration(_,t,_) => s"${indent}.captured ${evalType(t)} $a~$scope$endl"
    s"${globals.mkString}${locals.mkString}${params.mkString}${captured.mkString}"

  private def evalCodes(nodes: List[Code], indent: String): String =
    nodes.map(evalCode(_, indent)).mkString

  private def evalCode(node: Code, indent: String): String = node match
    case ThreeTac(op,d,l,r)        => threeTac(indent)(op,d,l,r)
    case TwoTac(op,d,v)            => twoTac(indent)(op,d,v)
    case OneTac(op,v)              => oneTac(indent)(op,v)
    case OneControl(op, dest)      => oneControl(indent)(op, dest)
    case TwoControl(op, src, dest) => twoControl(indent)(op, src, dest)
    case l: LabelIds               => labels(l)

  def oneTac[O](indent: String)(op: OneOperators, value: ASrc): String =
    val name = op match
      case RETURN => "return"
      case PUSH   => "push"
    s"${indent}$name ${const(value)}$endl"

  def twoTac[O](indent: String)(op: TwoOperators, dest: Variable, value: ASrc): String =
    val name = op match
      case ASSIGN             => "="
      case CALL               => "= call"
      case op: UnaryOperators => op.symbol
    s"${indent}${const(dest)} $name ${const(value)}$endl"

  def oneControl[O](indent: String)(op: OneControlOperators, value: LabelIds): String =
    val name = op match
      case JUMP => "jump"
    s"${indent}$name ${evalLabels(value)}$endl"

  def twoControl[O](indent: String)(op: TwoControlOperators, dest: ASrc, value: LabelIds): String =
    val name = op match
      case JUMP_IF_ZERO => "jump_if_zero"
    s"${indent}$name ${const(dest)} ${evalLabels(value)}$endl"

  def threeTac[O](indent: String)(op: ThreeOperators, dest: Variable, left: ASrc, right: ASrc): String =
    val name = op match
      case op: EqualityOperators       => op.symbol
      case op: RelationalOperators     => op.symbol
      case op: MultiplicativeOperators => op.symbol
      case op: AdditiveOperators       => op.symbol
    s"${indent}${const(dest)} = ${const(left)} $name ${const(right)}$endl"

  private def const(v: ASrc): String = v match
    case c: IntLiteral => c.toString
    case Scoped(i,s)   => s"$i~$s"
    case t: Temporary  => showTemporary(t)

  def evalLabels(v: LabelIds): String = v match
    case ElseLabel(c) => s"else$c"
    case Join(ic)     => s"join$ic"

  private def labels(v: LabelIds): String =
    s"${evalLabels(v)}:$endl"

  private def evalType(types: Types): String = types match
    case Cint => "%I32"
    case Cfunction => "%Function"
    case Cvoid => "%Void"
    case Cstring => "%String"
