package mmc

import Tac._
import Ast._

object Tac {
  type ThreeOperators = BinaryOperators
  type TwoOperators = UnaryOperators | MiscTwoOperators
  type OneOperators = MiscOneOperators
  type LabelIds = ElseLabel | Join
  type ASrc = Variable | Constant
  type Code = OneTac | TwoTac | ThreeTac | OneControl | TwoControl | LabelIds
  type Tac = Func
  type Global = Constant
}

enum OneControlOperators { case JUMP }
enum TwoControlOperators { case JUMP_IF_ZERO }
enum MiscTwoOperators { case ASSIGN, CALL }
enum MiscOneOperators { case RETURN, PUSH }
case class OneTac(op: OneOperators, a1: ASrc)
case class TwoTac(op: TwoOperators, dest: Variable, a1: ASrc)
case class ThreeTac(op: ThreeOperators, dest: Variable, a1: ASrc, a2: ASrc)
case class TwoControl(op: TwoControlOperators, a1: ASrc, label: LabelIds)
case class OneControl(op: OneControlOperators, label: LabelIds)
case class Func(id: Scoped, frame: Frame, body: List[Code])
case class ElseLabel(id: Long)
case class Join(id: Long)