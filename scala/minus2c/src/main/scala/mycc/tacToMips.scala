package mycc

import Ast._
import ArgList._
import exception._
import tacToMips._
import PseudoZero._
import PseudoUnary._
import MIPS._
import Temporaries._
import TwoAddr._
import scala.collection.mutable

object tacToMips extends Stage {
  type Source   = normalToTac.Goal
  type Context  = MipsContext
  type Goal     = List[Assembler]

  private val main = Identifier("main")
  private val predef = List(Globl(main), Text): Goal

  private type MipsAcc = (Context, Goal, Source)

  def apply(context: normalToTac.Context, tac: Source): (Context, Goal) =
    new tacToMips().goal(MipsContext(Cursor(Nil, Map(), context), None), tac)

  case class MipsContext(cursor: Cursor, private val _temporary: Option[Temporaries]) {
    def advanceTemporary: MipsContext = {
      val temp = _temporary.map { t =>
        if (t.enumTag == Temporaries.enumValue.size) {
          throw SemanticError("Too many temporaries!")
        }
        Temporaries.enumValue(t.enumTag + 1)
      } orElse {
        Some(T0)
      }
      MipsContext(cursor, temp)
    }

    def temporary: Temporaries = _temporary.getOrElse { T0 }
  }
}

class tacToMips {
  val data = new mutable.AnyRefMap[Identifier, List[Assembler]]() // only for globals

  private def goal(context: Context, tac: normalToTac.Goal): (Context, Goal) = {
    val topLevel = context.cursor.current
    topLevel.local(main) match {
      case Some(Declaration(auto, int, FunctionDeclarator(`main`, LVoid)))
        if topLevel.definition(main).isDefined =>

          var (contextAcc: Context, codeAcc: Goal, restAcc: Source) =
            (context, Nil, tac)

          while (!restAcc.isEmpty) {
            val (context, code, rest) = topLevelStatements(contextAcc, restAcc)
            codeAcc = code ++ codeAcc
            restAcc = rest
            contextAcc = context
          }

          val dataFinal: Goal =
            if (data.isEmpty)
              Nil
            else
              Data :: data.values.foldLeft(Nil: Goal)(_++_)
              
          val codeFinal: Goal = codeAcc ++ dataFinal
          (context, predef ++ codeFinal) // TODO: replace topLevel with context.current once stacked pops off frame
      case _ =>
        throw SemanticError("function definition for `int main(void)` not found.")
    }
  }

  private def topLevelStatements(context: Context, tac: Source): MipsAcc = tac match {
    case Function(id, body) :: stack =>
      var (contextAcc: Context, codeAcc: Goal, restAcc: Source) = (context, Nil, body)
      while (!restAcc.isEmpty) {
        val (newContext, code, rest) = evalStatements(contextAcc, restAcc)
        codeAcc = code ++ codeAcc
        restAcc = rest
        contextAcc = newContext
      }
      (contextAcc, Label(id) :: codeAcc.reverse, restAcc ++ stack)
    case _ => (context, Nil, Nil)
  }

  private def evalStatements(context: Context, tac: Source): MipsAcc = {
    tac match {
      case Assignment(_, c: Constant) :: rest =>
        val advanced = context.advanceTemporary
        val mips = Li(advanced.temporary, c)
        (advanced, List(mips), rest)
      // case t @ Temporary(inner) if isAtomic(inner) =>
      //   Nil
      // case Assignment(id, inner) if isAtomic(inner) =>
      //   Nil
      // case r @ Return(inner :: Nil) if isAtomic(inner) =>
      //   Nil
      case _ :: rest => (context, Nil, rest) // yield none, consume 1
      case Nil => (context, Nil, Nil) // yield none, consume 0
    }
  }

  // private def isAtomic(node: Assignments): Boolean = {
  //   node match {
  //     case v @ (
  //       _: Equality
  //     | _: Relational
  //     | _: Additive
  //     | _: Multiplicative
  //     | _: Unary
  //     | _: Constant
  //     | _: StringLiteral
  //     | _: Identifier
  //     | _: Relational
  //     ) =>
  //       true
  //     case Temporary(t) if isAtomic(t) =>
  //       true
  //     case _ =>
  //       false
  //   }
  // }
}