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
import normalToTac._

object printTac {

  private val endl = System.lineSeparator

  def apply(context: Bindings, src: (DataMap, List[Tac])): Unit =
    print(tac(context, src, "  "))

  private def tac
    ( context: Bindings,
      src: (DataMap, List[Tac]),
      indent: String
    ): String = {
      val (data, nodes) = src
      val dataString = s"$indent.static$endl"
      val codeString = s"$indent.code$endl"
      val dataAll = evalData(context, data, indent)
      val codeAll = evalNodes(context, nodes, indent)
      dataString + dataAll + codeString + codeAll
    }

  private def evalData
    ( context: Bindings,
      data: DataMap,
      indent: String
    ): String = data.view.map {
        case (Identifier(i),Constant(c)) =>
          s"$i:$endl${indent}.const %I32 $c$endl"
    }.mkString

  private def evalNodes
    ( context: Bindings,
      nodes: List[Tac],
      indent: String
    ): String =
      (for (node <- nodes.view)
          yield evalNode(context, node, indent)
      ).mkString
  
  private def evalNode
    ( context: Bindings,
      node: Tac,
      indent: String
    ): String =
      node match {
        case Func(identifier,frame,codes) =>
          evalFunction(context, identifier, frame, codes, indent)
      }

  private def evalFunction
    ( context: Bindings,
      identifier: Identifier,
      frame: Frame,
      codes: List[Code],
      indent: String
    ): String = {
      s"${identifier.id}:$endl" +
      s"$indent.begin_function$endl" +
      evalFrame(context, frame, indent) +
      evalCodes(context, codes, indent) +
      s"$indent.end_function$endl"
    }
    
  private def evalFrame
    ( context: Bindings,
      frame: Frame,
      indent: String
    ): String = {
      val globals = frame.globals.view.map {
        case (Identifier(a), Declaration(_,t,_)) =>
          s"${indent}.global ${evalType(t)} $a$endl"
      }.mkString
      val locals = frame.locals.view.map {
        case ((Identifier(a), scope), Declaration(_,t,_)) =>
          s"${indent}.local ${evalType(t)} $a~$scope$endl"
      }.mkString
      val params = frame.params.view.map {
        case ((Identifier(a), scope), Declaration(_,t,_)) =>
          s"${indent}.param ${evalType(t)} $a~$scope$endl"
      }.mkString
      val captured = frame.captures.view.map {
        case ((Identifier(a), scope), Declaration(_,t,_)) =>
          s"${indent}.captured ${evalType(t)} $a~$scope$endl"
      }.mkString
      globals + locals + params + captured
    }

  private def evalCodes
    ( context: Bindings,
      nodes: List[Code],
      indent: String
    ): String =
      (for (code <- nodes.view)
          yield evalCode(context, code, indent)
      ).mkString

  private def evalCode
    ( context: Bindings,
      node: Code,
      indent: String
    ): String = {
      node match {
        case ThreeTac(op,d,l,r) =>
          threeTac(indent)(op,d,l,r)
        case TwoTac(op,d,v) =>
          twoTac(indent)(op,d,v)
        case OneTac(op,v) =>
          oneTac(indent)(op,v)
      }
    }

  def oneTac[O]
    (indent: String)
    (op: OneOperators, value: ASrc): String = {
      val name = op match {
        case RETURN => "return"
        case PUSH_PARAM => "push"
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
    case Identifier(i) => i
    case t: Temporary => showTemporary(t)
  }

  private def evalType(types: Types): String = types match {
    case Cint => "%I32"
    case Cfunction => "%Function"
    case Cvoid => "%Void"
    case Cstring => "%String"
  }
}