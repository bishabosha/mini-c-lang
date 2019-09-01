package mmc

import MIPS._
import Tac._

object MIPS {
  type Labels = Label | ControlLabel
  type Src = Register | Constant
  type Register = Results | Arguments | Temporaries | SavedValues | Trap | Misc
  type Addresses = OffsetAddress | Labels
  type Dest = Addresses | Register
  type Assembler = ZeroAddr | OneAddr | TwoAddr | ThreeAddr | PseudoZero |
       PseudoUnary | Labels | Comment
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
  case Jal(dest: Labels)
  case Jr(dest: Register)
  case J(dest: Labels)
}

enum TwoAddr {
  case Beqz(source: Register, breakTo: Labels)
  case Move(dest: Register, source: Register)
  case Li(dest: Register, source: Constant)
  case Lw(dest: Register, source: Addresses)
  case La(dest: Register, source: Addresses)
  case Sw(source: Register, dest: Addresses)
  case Not(dest: Register, r: Src)
  case Neg(dest: Register, r: Src)
}

enum ThreeAddr {
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

case class ControlLabel(id: LabelIds)
case class Label(id: Scoped)
case class OffsetAddress(address: Register, offset: Constant)

enum PseudoZero {
  case Text, Data
}

enum PseudoUnary {
  case Word(size: Constant)
  case Globl(name: Scoped)
  case Asciiz(value: String)
}

case class Comment(msg: String)