package mycc

import Ast._
import ArgList._
import astToNormal._
import normalToTacActual._
import exception._
import StorageTypes._
import Types._
import Tac._
import MiscTwoOperators._
import MiscOneOperators._

object normalToTacActual extends Stage {
  type Source   = astToNormal.Goal
  type Context  = astToNormal.Context
  type Goal     = (DataMap, List[Tac])

  type DataMap = Map[Identifier, GlobalData]

  def apply(topLevel: Context, nodes: List[Statements]): (Context, Goal) =
    parseMain(topLevel) { f =>
      val data = nodes.foldLeft(Map(): Map[Identifier, GlobalData]) {
        (acc, statement) =>
          getData(statement).fold(acc)(acc + _)
        }
      val code = nodes.foldLeft(Nil: List[Tac]){ (code, statement) =>
        topLevelStatement(statement) ++ code
      }.reverse
      (topLevel, (data, code))
    }

  private def getData(node: Statements): Option[(Identifier, GlobalData)] =
    node match {
      case Declaration(_, _, i: Identifier) =>
        Some((i, GlobalConstant(i, zero)))
      case Assignment(i: Identifier, c: Constant) =>
        Some((i, GlobalConstant(i, c)))
      case _ => None
    }

  private def topLevelStatement(node: Statements): List[Tac] = node match {
    case Function(Std.`mainIdentifier`, f, body) =>
        val validated = body.foldLeft(Nil: List[Code]){ (code, statement) =>
          evalStatement(statement) ++ code
        }
        List(Func(Std.mainIdentifier, f, validated.reverse))
    case _ => Nil
  }

  private def evalStatement(node: Statements): List[Code] =
    node match {
      case Assignment(dest, expr: ExpressionRoot) =>
        evalExpr(dest,expr)
      case Return((a: ASrc) :: Nil) =>
        List(OneTac(RETURN, a))
      // case Return((expr: ExpressionRoot) :: Nil) =>
      //   val temp = new Temporary
      //   OneTac(RETURN, temp) :: evalExpr(temp,expr)
      case _ =>
        List()
    }

  private def evalExpr(dest: Variable, expr: ExpressionRoot): List[Code] = {
    import AssignmentsPattern._
    expr match {
      case Binary(op: BinaryOperators, l: ASrc, r: ASrc) =>
        ThreeTac(op, dest, l, r) :: Nil
      case Unary(op, v: ASrc) =>
        TwoTac(op, dest, v) :: Nil
      case b: ASrc =>
        TwoTac(ASSIGN, dest, b) :: Nil
      case _ =>
        Nil
    }
  }
}