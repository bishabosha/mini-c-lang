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
import AdditiveOperators._
import scala.collection.mutable

object tacToMips extends Stage {
  type Source   = normalToTac.Goal
  type Context  = MipsContext
  type Goal     = List[Assembler]

  private val main = Identifier("main")
  private val predef = List(Globl(main), Text): Goal

  private type MipsAcc = (Context, Goal, Source)
  private type Stack = List[Assembler]

  def apply(context: normalToTac.Context, tac: Source): (Context, Goal) =
    goal(MipsContext(Cursor(Nil, Map(), context), Map(), None), tac)

  case class MipsContext
    (
      cursor: Cursor,
      data: Map[Identifier, List[Assembler]],
      private val _temporary: Option[Temporaries]
    ) {
      def advanceTemporary: MipsContext = {
        val temp = _temporary.map { t =>
          if (t.enumTag == Temporaries.enumValue.size) {
            throw SemanticError("Too many temporaries!")
          }
          Temporaries.enumValue(t.enumTag + 1)
        } orElse {
          Some(T0)
        }
        MipsContext(cursor, data, temp)
      }

      def temporary: Temporaries = _temporary.getOrElse { T0 }
    }

  private def goal
    ( context: Context,
      tac: normalToTac.Goal
    ): (Context, Goal) = {
      val topLevel = context.cursor.current
      topLevel.local(main) match {
        case Some(Declaration(auto, int, FunctionDeclarator(`main`, LVoid)))
          if topLevel.definition(main).isDefined =>
            foldCode(context, Nil, tac)(topLevelStatements)(appendData)
        case _ =>
          throw SemanticError(
            "function definition for `int main(void)` not found.")
      }
    }

  private def topLevelStatements
    ( context: Context,
      code: Goal,
      tac: Source
    ): MipsAcc = tac match {
      case Function(id, body) :: stack =>
        foldCode(context, Nil, body)(evalStatements){ (context,code,rest) =>
          (context, Label(id) :: code.reverse, rest ++ stack)
        }
      case _ => (context, Nil, Nil)
    }

  private def evalStatements
    ( context: Context,
      code: Goal,
      tac: Source
    ): MipsAcc = {
      tac match {
        case Assignment(_, inner) :: rest => // please keep track of which temps contain vars
          evalExpr(context, None, inner, Nil, rest)
        case Temporary(inner) :: rest =>
          evalExpr(context, None, inner, Nil, rest)
        // case t @ Temporary(inner) if isAtomic(inner) =>
        //   Nil
        // case Assignment(id, inner) if isAtomic(inner) =>
        //   Nil
        // case r @ Return(inner :: Nil) if isAtomic(inner) =>
        //   Nil
        case _ :: rest => (context, Nil, rest) // yield rest, consume 1
        case Nil => (context, Nil, Nil) // yield none, consume 0
      }
    }

  private def evalExpr
    ( context: Context,
      dest: Option[Register],
      expr: Statements,
      stack: Stack,
      tac: Source
    ): MipsAcc = {
      (expr, dest) match {
        case (c: Constant, None) =>
          val advanced = context.advanceTemporary
          val mips = Li(advanced.temporary, c)
          (advanced, List(mips), tac)
        case (Additive(op, _, _), None) => op match { // need to pass in stack of already generated code to reuse registers for binary op
          case PLUS =>
            (context, Nil, tac)
          case MINUS =>
            (context, Nil, tac)
        }
        case _ =>
          (context, Nil, tac)
      }
    }

  private def foldCode[O]
    ( acc: MipsAcc )
    ( f: MipsAcc => MipsAcc )
    ( finisher: MipsAcc => O
    ) : O = {
      var (contextAcc, codeAcc, restAcc) = acc
      while (!restAcc.isEmpty) {
        val (context, code, rest) = f(contextAcc, codeAcc, restAcc)
        codeAcc = code ++ codeAcc
        restAcc = rest
        contextAcc = context
      }
      finisher(contextAcc, codeAcc, restAcc)
    }
  
  private def appendData
    ( context: Context,
      code: Goal,
      tac: Source
    ): (Context, Goal) = {
      val dataFinal: Goal =
        if (context.data.isEmpty)
          Nil
        else
          Data :: context.data.values.foldLeft(Nil: Goal)(_++_)          
      val codeFinal: Goal = code ++ dataFinal
      (context, predef ++ codeFinal)  // TODO: replace topLevel with context.current once stacked pops off frame
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