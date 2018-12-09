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
        case (Identifier(a), Declaration(_,Cint,_)) =>
          s"${indent}.global %I32 $a$endl"
        case (Identifier(a), Declaration(_,Cfunction,_)) =>
          s"${indent}.global %Function $a$endl"
      }.mkString
      val locals = frame.locals.view.map {
        case ((Identifier(a), scope), Declaration(_,Cint,_)) =>
          s"${indent}.local %I32 $a~$scope$endl"
        case ((Identifier(a), scope), Declaration(_,Cfunction,_)) =>
          s"${indent}.local %Function $a~$scope$endl"
      }.mkString
      val params = frame.params.view.map {
        case ((Identifier(a), scope), Declaration(_,Cint,_)) =>
          s"${indent}.param %I32 $a~$scope$endl"
        case ((Identifier(a), scope), Declaration(_,Cfunction,_)) =>
          s"${indent}.param %Function $a~$scope$endl"
      }.mkString
      val captured = frame.captures.view.map {
        case ((Identifier(a), scope), Declaration(_,Cint,_)) =>
          s"${indent}.captured %I32 $a~$scope$endl"
        case ((Identifier(a), scope), Declaration(_,Cfunction,_)) =>
          s"${indent}.captured %Function $a~$scope$endl"
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
      }
      s"${indent}$name ${const(value)}$endl"
    }

  def twoTac[O]
    (indent: String )
    (op: TwoOperators, dest: Variable, value: ASrc): String = {
      val name = op match {
        case ASSIGN => "assign"
        case NOT => "not"
        case POSITIVE => "positive"
        case NEGATIVE => "negative"
      }
      s"${indent}$name ${const(dest)}, ${const(value)}$endl"
    }

  def threeTac[O]
    (indent: String)
    (op: ThreeOperators, dest: Variable, left: ASrc, right: ASrc): String = {
      val name = op match {
        case EQUAL => "equal"
        case NOT_EQUAL => "not_equal"
        case LT => "less_than"
        case GT => "greater_than"
        case GT_EQ => "greater_than_or_equal"
        case LT_EQ => "less_than_or_equal"
        case PLUS => "plus"
        case MINUS => "minus"
        case MULTIPLY => "multiply"
        case DIVIDE => "divide"
        case MODULUS => "modulus"
      }
      s"${indent}$name ${const(dest)}, ${const(left)}, ${const(right)}$endl"
    }

  private def const(v: ASrc): String = v match {
    case Constant(c) => c.toString
    case Identifier(i) => i
    case t: Temporary => showTemporary(t)
  }
}