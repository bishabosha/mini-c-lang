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
            foldCode(context, tac)(topLevelStatements)(appendData)
        case _ =>
          throw SemanticError(
            "function definition for `int main(void)` not found.")
      }
    }

  private def topLevelStatements
    ( context: Context,
      tac: Source
    ): MipsAcc = tac match {
      case Function(id, body) :: rest =>
        evalFunction(context, id, body, rest)
      case _ => (context, Nil, Nil)
    }

  private def evalStatements
    ( context: Context,
      tac: Source
    ): MipsAcc = {
      tac match {
        case Assignment(id, inner) :: rest =>
          evalExpr(context, id, inner, None, Nil, rest)
        case (t @ Temporary(inner)) :: rest =>
          evalExpr(context, t, inner, None, Nil, rest)
        case _ :: rest => (context, Nil, rest) // yield rest, consume 1
        case Nil => (context, Nil, Nil) // yield none, consume 0
      }
    }

  private def evalExpr
    ( context: Context,
      variable: Identifier | Temporary,
      value: Statements,
      dest: Option[Register],
      stack: Stack,
      rest: Source
    ): MipsAcc = {
      (value, dest) match {
        case (c: Constant, None) =>
          evalConstant(context, variable, c, rest)
        case (Additive(op, _, _), None) =>
          evalAdditive(context, op, rest)
        case _ =>
          (context, Nil, rest)
      }
    }

  private def evalFunction
    ( context: Context,
      id: Identifier,
      body: Source,
      rest: Source
    ): MipsAcc =
      foldCode(context, body)(evalStatements) { (context,code) =>
        (context, Label(id) :: code.reverse, rest)
      }

  private def evalConstant
    ( context: Context,
      variable: Identifier | Temporary,
      c: Constant,
      rest: Source
    ): MipsAcc = {
      val advanced = context.advanceTemporary
      val statement = Li(advanced.temporary, c)
      (advanced, List(statement), rest)
    }

  private def evalAdditive
    ( context: Context,
      op: AdditiveOperators,
      rest: Source
    ): MipsAcc = op match { // need to pass in stack of already generated code to reuse registers for binary op
      case PLUS =>
        (context, Nil, rest)
      case MINUS =>
        (context, Nil, rest)
    }

  private def foldCode[O]
    ( acc: (Context, Source) )
    ( f: (Context, Source) => MipsAcc )
    ( finisher: (Context, Goal) => O
    ) : O = {
      var (contextAcc, restAcc) = acc
      var codeAcc: Goal = Nil
      while (!restAcc.isEmpty) {
        val (context, code, rest) = f(contextAcc, restAcc)
        codeAcc = code ++ codeAcc
        restAcc = rest
        contextAcc = context
      }
      finisher(contextAcc, codeAcc)
    }
  
  private def appendData
    ( context: Context,
      code: Goal
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