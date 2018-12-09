package mycc

import Tac._
import Ast._

object Tac {
  type ThreeOperators = EqualityOperators | RelationalOperators |
    AdditiveOperators | MultiplicativeOperators
  type TwoOperators = UnaryOperators | MiscTwoOperators
  type OneOperators = MiscOneOperators

  type RSrc = Identifier | Temp
  type ASrc = RSrc | Constant

  type GlobalData = GlobalWord
  type Code = OneTac | TwoTac | ThreeTac
  type Tac = Func
  type LDest = Temp | Identifier
}

class Temp // TODO: fix nesting
enum MiscTwoOperators { case ASSIGN }
enum MiscOneOperators { case RETURN }
case class GlobalWord(name: Identifier, value: Constant)
case class OneTac(op: OneOperators, a1: ASrc)
case class TwoTac(op: TwoOperators, dest: LDest, a1: ASrc)
case class ThreeTac(op: ThreeOperators, dest: LDest, a1: ASrc, a2: ASrc)
case class Func(id: Identifier, frame: Frame, body: List[Code])