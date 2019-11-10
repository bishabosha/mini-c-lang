package mmc

import Ast._

object normalToInterpreter extends Stage
  type Source  = astToNormal.Goal
  type Context = astToNormal.Context
  type Goal    = astToNormal.Goal

  def apply(nodes: Source): Goal =
    nodes.foldRight(List.empty[Declarations])(topLevelStatement(_) ::: _)

  private def topLevelStatement(node: Declarations): Goal = node match
    case Function(Std.`mainIdentifier`, f, body) =>
      val validated = body.foldRight(List.empty[Statements])(evalStatement(_) ::: _)
      List(Function(Std.mainIdentifier, f, validated))
    case a: Assignment => List(a)
    case d: Declaration => List(d)
    case _ => Nil

  private def evalStatement(node: Statements): List[Statements] = node match
    case d @ Declaration(_, _, _: Scoped)       => List(d)
    case a @ Assignment(_, _: ExpressionRoot)   => List(a)
    case r @ Return((_: ExpressionRoot) :: Nil) => List(r)
    case _                                      => List()
