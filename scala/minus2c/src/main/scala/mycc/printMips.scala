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
import Misc._
import PseudoZero._
import PseudoUnary._
import ZeroAddr._
import OneAddr._
import TwoAddr._
import ThreeAddr._
import exception._

object printMips {

  private val endl = System.lineSeparator

  def apply(nodes: List[Assembler]): Unit =
    print(mipsNode(nodes, "  "))

  private def mipsNode
    ( nodes: List[Assembler],
      indent: String
    ): String = {
      (for (node <- nodes.view) yield astNode(node, indent)).mkString
    }

  private def astNode
    ( node: Assembler,
      indent: String
    ): String = {
      node match {
        case Text =>
          s"$indent.text$endl"
        case Comment(msg) =>
          s"$indent#$msg$endl"
        case Data =>
          s"$indent.data$endl"
        case Globl(Identifier(id)) =>
          s"$indent.globl $id$endl"
        case Label(Identifier(i)) =>
          s"$i:$endl"
        case Word(Constant(w)) =>
          s"$indent.word $w$endl"
        case Syscall =>
          s"${indent}syscall$endl"
        case jal: Jal =>
          oneAddr(jal,indent)(_.dest)
        case jr: Jr =>
          oneAddr(jr,indent)(_.dest)
        case li: Li =>
          twoAddr(li,indent)(_.dest,_.source)
        case lw: Lw =>
          twoAddr(lw,indent)(_.dest,_.source)
        case neg: Neg =>
          twoAddr(neg,indent)(_.dest,_.r)
        case not: Not =>
          twoAddr(not,indent)(_.dest,_.r)
        case move: Move =>
          twoAddr(move,indent)(_.dest,_.source)
        case sw: Sw =>
          twoAddr(sw,indent)(_.source,_.dest)
        case add: Add =>
          threeAddr(add,indent)(_.dest,_.l,_.r)
        case sub: Sub =>
          threeAddr(sub,indent)(_.dest,_.l,_.r)
        case mul: Mul =>
          threeAddr(mul,indent)(_.dest,_.l,_.r)
        case div: Div =>
          threeAddr(div,indent)(_.dest,_.l,_.r)
        case rem: Rem =>
          threeAddr(rem,indent)(_.dest,_.l,_.r)
        case seq: Seq =>
          threeAddr(seq,indent)(_.dest,_.l,_.r)
        case sne: Sne =>
          threeAddr(sne,indent)(_.dest,_.l,_.r)
        case slt: Slt =>
          threeAddr(slt,indent)(_.dest,_.l,_.r)
        case sgt: Sgt =>
          threeAddr(sgt,indent)(_.dest,_.l,_.r)
        case sle: Sle =>
          threeAddr(sle,indent)(_.dest,_.l,_.r)
        case sge: Sge =>
          threeAddr(sge,indent)(_.dest,_.l,_.r)
        case _ => s"${indent}???$endl"
      }
    }

  def oneAddr[O]
    ( a: O, indent: String)
    ( r: O => Dest,
    ): String = {
      val name = a.getClass.getSimpleName.toLowerCase
      s"${indent}$name ${rsrc(r(a))}$endl"
    }

  def twoAddr[O]
    ( a: O, indent: String)
    ( d: O => Register,
      r: O => Dest | Constant
    ): String = {
      val name = a.getClass.getSimpleName.toLowerCase
      s"${indent}$name ${registers(d(a))}, ${rsrc(r(a))}$endl"
    }

  def threeAddr[O]
    ( a: O,
      indent: String )
    ( d: O => Register,
      l: O => Register,
      r: O => Src
    ): String = {
      val name = a.getClass.getSimpleName.toLowerCase
      s"${indent}$name ${registers(d(a))}, ${registers(l(a))}, ${rsrc(r(a))}$endl"
    }

  private def rsrc(v: Constant | Dest): String = v match {
    case Constant(c) => c.toString
    case Label(Identifier(i)) => i
    case r: Register => registers(r)
    case u =>
      throw UnexpectedAstNode(u.toString)
  }

  def registers(reg: Register): String = reg match {
    case t: Temporaries =>
      printEnum(Temporaries.enumValueNamed, t, "$t")
    case s: SavedValues =>
      printEnum(SavedValues.enumValueNamed, s, "$s")
    case r: Results =>
      printEnum(Results.enumValueNamed, r, "$v")
    case a: Arguments =>
      printEnum(Arguments.enumValueNamed, a, "$a")
    case Ra =>
      "$ra"
    case _ =>
      "$?_"
  }

  def printEnum[E](e: Map[String,Enum], t: E, code: String) = {
    val num = e(t.toString).enumTag
      s"$code$num"
  }
}