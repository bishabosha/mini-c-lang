package mycc

import MIPS._

object MIPS {
  type Src = Register | Constant
  type Register = Results | Arguments | Temporaries | SavedValues | Trap | Misc
  type Addresses = OffsetAddress | Label
  type Dest = Addresses | Register
  type Assembler = ZeroAddr | OneAddr | TwoAddr | ThreeAddr | PseudoZero | PseudoUnary | Label
}

enum Results {
  case V0, V1
}

enum Arguments {
  case A0, A1, A2, A3
}

enum Temporaries {
  case T0, T1, T2, T3, T4, T5, T6, T7, T8, T9
}

enum SavedValues {
  case S0, S1, S2, S3, S4, S5, S6, S7, S8
}

enum Trap {
  case K0, K1
}

enum Misc {
  case Zero, Sp, Gp, Fp, Ra
}

enum ZeroAddr {
  case Syscall
}

enum OneAddr {
  case Jal(dest: Label)
  case Jr(dest: Register)
  case J(dest: Label)
}

enum TwoAddr {
  case Move(dest: Register, source: Register)
  case Li(dest: Register, source: Constant)
  case Lw(dest: Register, source: Addresses)
  case La(dest: Register, source: Addresses)
  case Sw(source: Register, dest: Addresses)
  case Not(dest: Register, r: Src)
  case Neg(dest: Register, r: Src)
}

enum ThreeAddr {
  case Beq(l: Register, r: Register, breakTo: Identifier)
  case Add(dest: Register, l: Register, r: Src)
  case Sub(dest: Register, l: Register, r: Src)
  case Mul(dest: Register, l: Register, r: Src)
  case Div(dest: Register, l: Register, r: Src)
  case Rem(dest: Register, l: Register, r: Src)
  case Seq(dest: Register, l: Register, r: Src)
  case Sne(dest: Register, l: Register, r: Src)
  case Slt(dest: Register, l: Register, r: Src)
  case Sle(dest: Register, l: Register, r: Src)
  case Sgt(dest: Register, l: Register, r: Src)
  case Sge(dest: Register, l: Register, r: Src)
}

case class Label(id: Identifier)
case class OffsetAddress(address: Register, offset: Constant)

enum PseudoZero {
  case Text, Data
}

enum PseudoUnary {
  case Word(size: Constant)
  case Globl(name: Identifier)
  case Asciiz(value: String)
  case Comment(msg: String)
}

object AssignmentsPattern {
  import Ast._
  class Binary
    ( op: BinaryOp,
      l: Assignments,
      r: Assignments
    ) extends Product {
      def _1 = op
      def _2 = l
      def _3 = r

      // Not used by pattern matching: Product is only used as a marker trait.
      def canEqual(that: Any): Boolean = ???
      def productArity: Int = ???
      def productElement(n: Int): Any = ???
    }

  object Binary {
    type BinaryNode = Multiplicative | Additive | Relational | Equality
    def unapply
      (node: BinaryNode): Binary =
        node match {
          case Multiplicative(op,l,r) =>
            new Binary(op,l,r)
          case Additive(op,l,r) =>
            new Binary(op,l,r)
          case Relational(op,l,r) =>
            new Binary(op,l,r)
          case Equality(op,l,r) =>
            new Binary(op,l,r)
        }
  }
}