package mmc

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
import printTac._
import exception._

object printMips {

  private val endl = System.lineSeparator

  def apply(nodes: List[Assembler]): Unit = {
    var symbCount = 0L
    val symbols = new scala.collection.mutable.AnyRefMap[Scoped,Long]()

    print(mipsNode(nodes, "  "))

    def mipsNode
      ( nodes: List[Assembler],
        indent: String
      ): String = {
        (for (node <- nodes.view) yield astNode(node, indent)).mkString
      }

    def astNode
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
          case Globl(Scoped(Identifier(id),_)) =>
            s"$indent.globl $id$endl"
          case Label(Scoped(Identifier(i),-1)) =>
            s"$i:$endl"
          case Label(s @ Scoped(Identifier(i), id)) =>
            s"${getScopedLabel(s)}: #debug: $i~$id$endl"
          case ControlLabel(id) =>
            s"${evalLabels(id)}:$endl"
          case Word(Constant(w)) =>
            s"$indent.word $w$endl"
          case Syscall =>
            s"${indent}syscall$endl"
          case jal: Jal =>
            oneAddr(jal,indent)(_.dest)
          case jr: Jr =>
            oneAddr(jr,indent)(_.dest)
          case j: J =>
            oneAddr(j,indent)(_.dest)
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
          case beqz: Beqz =>
            twoAddr(beqz,indent)(_.source,_.breakTo)
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

    def rsrc(v: Constant | Dest): String = v match {
      case Constant(c) => c.toString
      case Label(Scoped(Identifier(i),-1)) => s"$i"
      case Label(s: Scoped) => getScopedLabel(s)
      case ControlLabel(id) => evalLabels(id)
      case r: Register => registers(r)
      case u =>
        throw UnexpectedAstNode(u.toString)
    }

    def registers(reg: Register): String = reg match {
      case t: Temporaries =>
        printEnum(Temporaries.valueOf, t, "$t")
      case s: SavedValues =>
        printEnum(SavedValues.valueOf, s, "$s")
      case r: Results =>
        printEnum(Results.valueOf, r, "$v")
      case a: Arguments =>
        printEnum(Arguments.valueOf, a, "$a")
      case Ra =>
        "$ra"
      case _ =>
        "$?_"
    }

    def getScopedId(s: Scoped): Long =
      symbols.getOrElseUpdate(s, { symbCount += 1; symbCount })

    def getScopedLabel(s: Scoped): String =
      "L" + getScopedId(s)

    def printEnum[E](e: String => Enum, t: E, code: String) = {
      val num = e(t.toString).ordinal
        s"$code$num"
    }
  }
}