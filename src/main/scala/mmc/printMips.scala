package mmc

import MIPS._
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
          case oa: OneAddr =>
            oneAddrNodes(oa, indent)
          case ta: TwoAddr =>
            twoAddrNodes(ta, indent)
          case ta: ThreeAddr =>
            threeAddrNodes(ta, indent)
          case _ => s"${indent}???$endl"
        }
      }

    def oneAddrNodes
      ( node: OneAddr, indent: String )
      : String = node match {
        case op: Jal =>
          oneAddr(op,indent)(_.prefixStr,_.dest)
        case op: Jr =>
          oneAddr(op,indent)(_.prefixStr,_.dest)
        case op: J =>
          oneAddr(op,indent)(_.prefixStr,_.dest)
      }

    def twoAddrNodes
      ( node: TwoAddr, indent: String )
      : String = node match {
        case op: Li =>
          twoAddr(op,indent)(_.prefixStr,_.dest,_.source)
        case op: Lw =>
          twoAddr(op,indent)(_.prefixStr,_.dest,_.source)
        case op: Neg =>
          twoAddr(op,indent)(_.prefixStr,_.dest,_.r)
        case op: Not =>
          twoAddr(op,indent)(_.prefixStr,_.dest,_.r)
        case op: Move =>
          twoAddr(op,indent)(_.prefixStr,_.dest,_.source)
        case op: Beqz =>
          twoAddr(op,indent)(_.prefixStr,_.source,_.breakTo)
        case op: Sw =>
          twoAddr(op,indent)(_.prefixStr,_.source,_.dest)
      }

    def threeAddrNodes
      ( node: ThreeAddr, indent: String )
      : String = node match {
        case op: Add =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Sub =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Mul =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Div =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Rem =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Seq =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Sne =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Slt =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Sle =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Sgt =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
        case op: Sge =>
          threeAddr(op,indent)(_.prefixStr,_.dest,_.l,_.r)
      }

    def oneAddr[O]
      ( a: O, indent: String)
      ( n: O => String,
        r: O => Dest,
      ): String =
        s"${indent}${n(a)} ${rsrc(r(a))}$endl"

    def twoAddr[O]
      ( a: O, indent: String)
      ( n: O => String,
        d: O => Register,
        r: O => Dest | Constant
      ): String =
        s"${indent}${n(a)} ${registers(d(a))}, ${rsrc(r(a))}$endl"

    def threeAddr[O]
      ( a: O,
        indent: String )
      ( n: O => String,
        d: O => Register,
        l: O => Register,
        r: O => Src
      ): String =
        s"${indent}${n(a)} ${registers(d(a))}, ${registers(l(a))}, ${rsrc(r(a))}$endl"

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
        printRegister(t,'t')
      case s: SavedValues =>
        printRegister(s,'s')
      case r: Results =>
        printRegister(r,'v')
      case a: Arguments =>
        printRegister(a,'a')
      case Ra =>
        "$ra"
      case _ =>
        "$?_"
    }

    def getScopedId(s: Scoped): Long =
      symbols.getOrElseUpdate(s, { symbCount += 1; symbCount })

    def getScopedLabel(s: Scoped): String =
      "L" + getScopedId(s)

    def printRegister[E <: Register](t: E, code: Char) = {
      "$" + code + t.ordinal
    }

    def (p: Product) prefixStr = p.productPrefix.toLowerCase
  }
}