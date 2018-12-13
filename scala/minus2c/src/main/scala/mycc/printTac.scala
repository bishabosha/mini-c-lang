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
import Tac._
import MiscTwoOperators._
import MiscOneOperators._
import TwoControlOperators._
import OneControlOperators._
import normalToTac._

object printTac {

  private val endl = System.lineSeparator

  def apply(context: DataMap, src: List[Tac]): Unit =
    print(tac(context, src, "  "))

  private def tac
    ( data: DataMap,
      src: List[Tac],
      indent: String
    ): String = {
      val dataString = s"$indent.static$endl"
      val codeString = s"$indent.code$endl"
      val dataAll = evalData(data, indent)
      val codeAll = evalNodes(src, indent)
      dataString + dataAll + codeString + codeAll
    }

  private def evalData
    ( data: DataMap,
      indent: String
    ): String = data.view.map {
        case (Scoped(Identifier(i),s),Constant(c)) =>
          s"$i~$s:$endl${indent}.const %I32 $c$endl"
    }.mkString

  private def evalNodes
    ( nodes: List[Tac],
      indent: String
    ): String =
      (for (node <- nodes.view)
          yield evalNode(node, indent)
      ).mkString
  
  private def evalNode
    ( node: Tac,
      indent: String
    ): String =
      node match {
        case Func(scoped, frame, codes) =>
          evalFunction(scoped, frame, codes, indent)
      }

  private def evalFunction
    ( scoped: Scoped,
      frame: Frame,
      codes: List[Code],
      indent: String
    ): String = {
      s"${scoped.id.id}~${scoped.scope}:$endl" +
      s"$indent.begin_function$endl" +
      evalFrame(frame, indent) +
      evalCodes(codes, indent) +
      s"$indent.end_function$endl"
    }
    
  private def evalFrame
    ( frame: Frame,
      indent: String
    ): String = {
      val globals = frame.globals.view.map {
        case (Identifier(a), Declaration(_,t,_)) =>
          s"${indent}.global ${evalType(t)} $a~0$endl"
      }.mkString
      val locals = frame.locals.view.map {
        case (Scoped(Identifier(a), scope), Declaration(_,t,_)) =>
          s"${indent}.local ${evalType(t)} $a~$scope$endl"
      }.mkString
      val params = frame.params.view.map {
        case (Scoped(Identifier(a), scope), Declaration(_,t,_)) =>
          s"${indent}.param ${evalType(t)} $a~$scope$endl"
      }.mkString
      val captured = frame.captures.view.map {
        case (Scoped(Identifier(a), scope), Declaration(_,t,_)) =>
          s"${indent}.captured ${evalType(t)} $a~$scope$endl"
      }.mkString
      globals + locals + params + captured
    }

  private def evalCodes
    ( nodes: List[Code],
      indent: String
    ): String =
      (for (code <- nodes.view)
          yield evalCode(code, indent)
      ).mkString

  private def evalCode
    ( node: Code,
      indent: String
    ): String = {
      node match {
        case ThreeTac(op,d,l,r) =>
          threeTac(indent)(op,d,l,r)
        case TwoTac(op,d,v) =>
          twoTac(indent)(op,d,v)
        case OneTac(op,v) =>
          oneTac(indent)(op,v)
        case OneControl(op, dest) =>
          oneControl(indent)(op, dest)
        case TwoControl(op, src, dest) =>
          twoControl(indent)(op, src, dest)
        case l: LabelIds =>
          labels(l)
      }
    }

  def oneTac[O]
    (indent: String)
    (op: OneOperators, value: ASrc): String = {
      val name = op match {
        case RETURN => "return"
        case PUSH => "push"
      }
      s"${indent}$name ${const(value)}$endl"
    }

  def twoTac[O]
    (indent: String )
    (op: TwoOperators, dest: Variable, value: ASrc): String = {
      val name = op match {
        case ASSIGN => "="
        case NOT => "= !"
        case POSITIVE => "= +"
        case NEGATIVE => "= -"
        case CALL => "= call"
      }
      s"${indent}${const(dest)} $name ${const(value)}$endl"
    }

  def oneControl[O]
    (indent: String )
    (op: OneControlOperators, value: LabelIds): String = {
      val name = op match {
        case JUMP => "jump"
      }
      s"${indent}$name ${evalLabels(value)}$endl"
    }

  def twoControl[O]
    (indent: String )
    (op: TwoControlOperators, dest: ASrc, value: LabelIds): String = {
      val name = op match {
        case JUMP_IF_ZERO => "jump_if_zero"
      }
      s"${indent}$name ${const(dest)} ${evalLabels(value)}$endl"
    }

  def threeTac[O]
    (indent: String)
    (op: ThreeOperators, dest: Variable, left: ASrc, right: ASrc): String = {
      val name = op match {
        case EQUAL => "=="
        case NOT_EQUAL => "!="
        case LT => "<"
        case GT => ">"
        case GT_EQ => ">="
        case LT_EQ => ">="
        case PLUS => "+"
        case MINUS => "-"
        case MULTIPLY => "*"
        case DIVIDE => "/"
        case MODULUS => "%"
      }
      s"${indent}${const(dest)} = ${const(left)} $name ${const(right)}$endl"
    }

  private def const(v: ASrc): String = v match {
    case Constant(c) => c.toString
    case Scoped(Identifier(i),s) => s"$i~$s"
    case t: Temporary => showTemporary(t)
  }

  def evalLabels(v: LabelIds): String = v match {
    case ElseLabel(c) => s"else$c"
    case Join(ic) => s"join$ic"
  }

  private def labels(v: LabelIds): String =
    s"${evalLabels(v)}:$endl"

  private def evalType(types: Types): String = types match {
    case Cint => "%I32"
    case Cfunction => "%Function"
    case Cvoid => "%Void"
    case Cstring => "%String"
  }
}