package mycc

import MIPS._

object MIPS {
  type Register = Results | Arguments | Temporaries | SavedValues | Trap | Misc
  type Addresses = OffsetAddress | Identifier
}

enum Results {
  case $v0, $v1
}

enum Arguments {
  case $a0, $a1, $a2, $a3
}

enum Temporaries {
  case $t0, $t1, $t2, $t3, $t4, $t5, $t6, $t7, $t8, $t9
}

enum SavedValues {
  case $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $s8
}

enum Trap {
  case $k0, $k1
}

enum Misc {
  case zero, $sp, $gp, $fp, $ra
}

enum ZeroAddr {
  case syscall
}

enum OneAddr {
  case jal(dest: Identifier)
  case jr(dest: Register)
  case j(dest: Identifier)
}

enum TwoAddr {
  case move(dest: Register, source: Register)
  case li(dest: Register, source: Constant)
  case lw(dest: Register, source: Addresses)
  case la(dest: Register, source: Addresses)
  case sw(source: Register, dest: Addresses)
}

enum ThreeAddr {
  case addi(dest: Register, l: Register, r: Constant)
  case add(dest: Register, l: Register, r: Register)
  case sub(dest: Register, l: Register, r: Register)
  case mul(dest: Register, l: Register, r: Register)
  case beq(l: Register, r: Register, breakTo: Identifier)
}

case class Label(id: Identifier)
case class OffsetAddress(address: Register, offset: Constant)

enum PseudoUnary {
  case text, data
}

enum PsudeoBinary {
  case word(size: Constant)
  case globl(name: String)
  case asciiz(value: String)
}