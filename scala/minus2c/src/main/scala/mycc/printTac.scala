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
import normalToTacActual._

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
      val dataAll = tacData(context, data, indent)
      val codeAll = tacNodes(context, nodes, indent)
      dataString + dataAll + codeString + codeAll
    }

  private def tacNodes
    ( context: Bindings,
      nodes: List[Tac],
      indent: String
    ): String =
      nodes.view.map {
        case Func(Identifier(i),frame,codes) =>
          s"$i:$endl" +
          (for (code <- codes.view)
            yield codeNode(context, code, indent)
          ).mkString
      }.mkString

  private def tacData
    ( context: Bindings,
      data: DataMap,
      indent: String
    ): String = data.values.view.map {
        case GlobalConstant(Identifier(i),Constant(c)) =>
          s"$i:$endl${indent}.const $c$endl"
    }.mkString

  private def codeNode
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
        case ASSIGN => "set"
        case NOT => "not"
        case POSITIVE => "identity"
        case NEGATIVE => "negate"
      }
      s"${indent}$name ${const(dest)}, ${const(value)}$endl"
    }

  def threeTac[O]
    (indent: String)
    (op: ThreeOperators, dest: Variable, left: ASrc, right: ASrc): String = {
      val name = op match {
        case EQUAL => "eq"
        case NOT_EQUAL => "neq"
        case LT => "lt"
        case GT => "gt"
        case GT_EQ => "gte"
        case LT_EQ => "lte"
        case PLUS => "add"
        case MINUS => "sub"
        case MULTIPLY => "mul"
        case DIVIDE => "div"
        case MODULUS => "mod"
      }
      s"${indent}$name ${const(dest)}, ${const(left)}, ${const(right)}$endl"
    }

  private def const(v: ASrc): String = v match {
    case Constant(c) => c.toString
    case Identifier(i) => i
    case t: Temporary => showTemporary(t)
  }
}