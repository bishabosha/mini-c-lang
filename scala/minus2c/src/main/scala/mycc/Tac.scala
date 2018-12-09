package mycc

import Tac._
import Ast._

object Tac {
  type ThreeOperators = EqualityOperators | RelationalOperators |
    AdditiveOperators | MultiplicativeOperators
  type TwoOperators = UnaryOperators | MiscTwoOperators
  type OneOperators = MiscOneOperators

  type ASrc = Variable | Constant
  type Code = OneTac | TwoTac | ThreeTac
  type Tac = Func
  type Global = Constant
}

enum MiscTwoOperators { case ASSIGN }
enum MiscOneOperators { case RETURN }
// case class Global(name: Identifier, value: Constant)
case class OneTac(op: OneOperators, a1: ASrc)
case class TwoTac(op: TwoOperators, dest: Variable, a1: ASrc)
case class ThreeTac(op: ThreeOperators, dest: Variable, a1: ASrc, a2: ASrc)
case class Func(id: Identifier, frame: Frame, body: List[Code])