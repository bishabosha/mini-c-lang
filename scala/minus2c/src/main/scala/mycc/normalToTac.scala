package mycc

import Ast._
import ArgList._
import astToNormal._
import normalToTac._
import exception._
import StorageTypes._
import Types._
import Tac._
import MiscTwoOperators._
import MiscOneOperators._

object normalToTac extends Stage {
  type Source   = astToNormal.Goal
  type Context  = astToNormal.Context
  type Goal     = (DataMap, List[Tac])

  type DataMap = Map[Scoped, Global]

  def apply(topLevel: Context, nodes: List[Statements]): (Context, Goal) =
    parseMain(topLevel) { () =>
      val data = nodes.foldLeft(Map(): DataMap) {
        (acc, statement) =>
          getData(statement).fold(acc)(acc + _)
        }
      val code = nodes.foldLeft(Nil: List[Tac]){ (code, statement) =>
        topLevelStatement(statement) ++ code
      }.reverse
      (topLevel, (data, code))
    }

  private def getData(node: Statements): Option[(Scoped, Global)] =
    node match {
      case Declaration(_, _, i: Scoped) =>
        Some((i, zero))
      case Assignment(i: Scoped, c: Constant) =>
        Some((i, c))
      case _ => None
    }

  private def topLevelStatement
    (node: Statements): List[Tac] =
      node match {
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
      case Assignment(dest, Application(id: Scoped, args)) =>
        evalApplication(dest, id, args)
      case Return((a: ASrc) :: Nil) =>
        List(OneTac(RETURN, a))
      case _ =>
        List()
    }

  private def evalApplication
    (dest: Variable, id: Scoped, args: Expressions): List[Code] = {
      args.foldRight(List[Code](TwoTac(CALL, dest, id))) { (exp, acc) =>
        exp match {
          case a: ASrc =>
            OneTac(PUSH_PARAM, a) :: acc
          case _ =>
            acc
        }
      }.reverse
    }

  private def evalExpr(dest: Variable, expr: ExpressionRoot): List[Code] = {
    expr match {
      case Multiplicative(op, l: ASrc, r: ASrc) =>
        ThreeTac(op, dest, l, r) :: Nil
      case Additive(op, l: ASrc, r: ASrc) =>
        ThreeTac(op, dest, l, r) :: Nil
      case Relational(op, l: ASrc, r: ASrc) =>
        ThreeTac(op, dest, l, r) :: Nil
      case Equality(op, l: ASrc, r: ASrc) =>
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