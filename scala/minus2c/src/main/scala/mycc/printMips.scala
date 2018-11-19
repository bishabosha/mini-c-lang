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
import MIPS._
import tacToMips._
import PseudoZero._
import PseudoUnary._
import TwoAddr._
import ThreeAddr._
import exception._

object printMips {

  private val endl = System.lineSeparator

  def apply(context: MipsContext, nodes: List[Assembler]): Unit =
    print(mipsNode(context, nodes, "  "))

  private def mipsNode
    ( context: MipsContext,
      nodes: List[Assembler],
      indent: String
    ): String = {
      (for (node <- nodes.view) yield astNode(context, node, indent)).mkString
    }

  private def astNode
    ( context: MipsContext,
      node: Assembler,
      indent: String
    ): String = {
      def getIt(value: Assembler): String = astNode(context, value, indent);
      node match {
        case Text =>
          s"$indent.text$endl"
        case Data =>
          s"$indent.data$endl"
        case Globl(Identifier(id)) =>
          s"$indent.globl $id$endl"
        case Label(Identifier(i)) =>
          s"$i:$endl"
        case Word(w) =>
          s"$indent.word $w$endl"
        case li: Li =>
          twoAddr(li,indent)(_.dest,_.source)
        case neg: Neg =>
          twoAddr(neg,indent)(_.dest,_.r)
        case not: Not =>
          twoAddr(not,indent)(_.dest,_.r)
        case move: Move =>
          twoAddr(move,indent)(_.dest,_.source)
        case add: Add =>
          threeAddr(add,indent)(_.dest,_.l,_.r)
        case mul: Mul =>
          threeAddr(mul,indent)(_.dest,_.l,_.r)
        case div: Div =>
          threeAddr(div,indent)(_.dest,_.l,_.r)
        case seq: Seq =>
          threeAddr(seq,indent)(_.dest,_.l,_.r)
        case _ => s"${indent}???$endl"
      }
    }

  def twoAddr[O]
    ( a: O, indent: String)
    ( d: O => Register,
      r: O => Register | Constant
    ): String = {
      val name = a.getClass.getSimpleName.toLowerCase
      s"${indent}$name ${registers(d(a))}, ${regOrConst(r(a))}$endl"
    }

  def threeAddr[O]
    ( a: O,
      indent: String )
    ( d: O => Register,
      l: O => Register,
      r: O => Register | Constant
    ): String = {
      val name = a.getClass.getSimpleName.toLowerCase
      s"${indent}$name ${registers(d(a))}, ${registers(l(a))}, ${regOrConst(r(a))}$endl"
    }

  private def regOrConst(v: Any): String = v match {
    case Constant(c) => c.toString
    case _ => registers(v.asInstanceOf[Register])
  }

  private def registers(reg: Enum): String = reg match {
    case t: Temporaries =>
      printEnum(Temporaries.enumValueNamed, t, "$t")
    case s: SavedValues =>
      printEnum(SavedValues.enumValueNamed, s, "$s")
    case _ =>
      "$?_"
  }

  def printEnum[E](e: Map[String,Enum], t: E, code: String) = {
    val num = e(t.toString).enumTag
      s"$code$num"
  }
}