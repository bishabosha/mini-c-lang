package mycc

import MIPS._

object MIPS {
  type Register = Results | Arguments | Temporaries | SavedValues | Trap | Misc
  type Addresses = OffsetAddress | Identifier
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
  case Jal(dest: Identifier)
  case Jr(dest: Register)
  case J(dest: Identifier)
}

enum TwoAddr {
  case Move(dest: Register, source: Register)
  case Li(dest: Register, source: Constant)
  case Lw(dest: Register, source: Addresses)
  case La(dest: Register, source: Addresses)
  case Sw(source: Register, dest: Addresses)
}

enum ThreeAddr {
  case Addi(dest: Register, l: Register, r: Constant)
  case Add(dest: Register, l: Register, r: Register)
  case Sub(dest: Register, l: Register, r: Register)
  case Mul(dest: Register, l: Register, r: Register)
  case Beq(l: Register, r: Register, breakTo: Identifier)
}

case class Label(id: Identifier)
case class OffsetAddress(address: Register, offset: Constant)

enum PseudoZero {
  case Text, Data
}

enum PseudoUnary {
  case Word(size: Int)
  case Globl(name: Identifier)
  case Asciiz(value: String)
}