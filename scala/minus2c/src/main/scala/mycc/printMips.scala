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
        case Li(reg, Constant(c)) =>
          s"${indent}li ${registers(reg)} $c$endl"
        case _ => s"${indent}???$endl"
      }
    }

  private def registers(reg: Register): String = reg match {
    case t: Temporaries =>
      val num = Temporaries.enumValueNamed(t.toString).enumTag
      s"$$t$num"
  }
}