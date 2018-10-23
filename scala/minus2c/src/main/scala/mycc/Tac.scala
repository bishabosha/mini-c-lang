package mycc

import TacTypes._
import Ast._

object TacTypes {
  type Tac = I32 | Psuedo | Meta | Mem
  type Register = Int
  type Location = Register | Identifier
  type Value = Location | Constant
  type Psuedo = Declaration
}

enum I32 {
  case Binary(op: BinaryOp, dest: Register, l: Register, r: Register)
  case Unary(op: UnaryOp, dest: Register, l: Register)
}

enum Meta {
  case Label(id: Identifier)
}

enum Mem {
  case Load(dest: Register, v: Value)
  case Store(source: Register, v: Location)
}
