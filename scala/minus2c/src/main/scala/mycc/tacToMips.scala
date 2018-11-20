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
import ThreeAddr._
import AdditiveOperators._
import EqualityOperators._
import RelationalOperators._
import MultiplicativeOperators._
import UnaryOperators._
import StorageTypes._
import Types._
import scala.collection.mutable

object tacToMips extends Stage {
  type Source   = normalToTac.Goal
  type Context  = MipsContext
  type Goal     = List[Assembler]

  private val predef = List(Globl(Std.mainIdentifier), Text): Goal

  private type MipsAcc = (Context, Goal, Source)
  private type Stack = List[Assembler]

  private type BinaryArgs = (Register,Register,Src) => ThreeAddr
  private type UnaryArgs = (Register,Src) => TwoAddr

  private type MipsFor[Op] = Op match {
    case MultiplicativeOperators => PartialFunction[Op,BinaryArgs]
    case AdditiveOperators => PartialFunction[Op,BinaryArgs]
    case RelationalOperators => PartialFunction[Op,BinaryArgs]
    case EqualityOperators => PartialFunction[Op,BinaryArgs]
    case UnaryOperators => PartialFunction[Op,UnaryArgs]
  }

  def apply(context: normalToTac.Context, tac: Source): (Context, Goal) = {
    val cursor = Cursor.Empty.withBindings(context)
    val mipsContext = MipsContext(Nil, cursor, None, Set())
    goal(mipsContext, tac)
  }

  case class MipsContext
    (
      stack: List[MipsContext],
      cursor: Cursor,
      private val _temporary: Option[Temporaries],
      private val saved: Set[SavedValues]
    ) {
      def advanceTemporary: MipsContext = {
        val temp = _temporary.map { t =>
          if t.enumTag == Temporaries.enumValue.size then {
            throw SemanticError("Too many temporaries!")
          }
          Temporaries.enumValue(t.enumTag + 1)
        } orElse {
          Some(T0)
        }
        MipsContext(stack, cursor, temp, saved)
      }

      def advanceSaved: (SavedValues, MipsContext) = {
        val savedValues = SavedValues.enumValues.toSet
        val newSet = savedValues &~ saved
        if newSet.isEmpty then {
          throw SemanticError("Too many saved variables!")
        }
        val consumed = newSet.head
        (consumed, MipsContext(stack, cursor, _temporary, saved + consumed))
      }

      def temporary: Temporaries = _temporary.getOrElse { T0 }

      def +(key: Identifier | Temporary, value: Register): MipsContext = {
        val updated = cursor + (RegisterKey(key), value)
        MipsContext(stack, updated, _temporary, saved)
      }

      def next: Option[MipsContext] = cursor.next.map {
        MipsContext(this :: stack, _, None, Set())
      }
    }

  private def goal
    ( context: Context,
      tac: normalToTac.Goal
    ): (Context, Goal) = {
      val topLevel = context.cursor.current
      local(Std.mainIdentifier, topLevel) match {
        case Some(Std.`mainFunc`)
          if definition(Std.mainIdentifier,topLevel).isDefined =>
            val (contextFinal, goal) =
              foldCode(context.next.get,tac)(topLevelStatements)(identity(_,_))
            (contextFinal, predef ++ goal)
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

  private def evalFunction
    ( context: Context,
      id: Identifier,
      body: Source,
      rest: Source
    ): MipsAcc =
      foldCode(defineLocals(context),body)(evalStatements) {
        (context,code) => (context,Label(id) :: code.reverse,rest)
      }

  private def defineLocals(context: Context): Context =
    locals(context.cursor.current).foldLeft(context) { (c, kv) =>
      kv match {
        case Declaration(_, _, i: Identifier) => 
          val (register, advanced) = c.advanceSaved
          advanced + (i, register)
        case _ =>
          c
      }
    }

  private def evalStatements
    ( context: Context,
      tac: Source
    ): MipsAcc = tac match {
      case Assignment(id, inner) :: rest =>
        assignExpr(context, id, inner, rest)
      case (t @ Temporary(inner)) :: rest =>
        assignExpr(context, t, inner, rest)
      case _ :: rest => (context, Nil, rest) // yield rest, consume 1
      case Nil => (context, Nil, Nil) // yield none, consume 0
    }

  private def assignExpr
    ( context: Context,
      lvalue: Identifier | Temporary,
      value: Statements,
      rest: Source
    ): MipsAcc = value match {
      case c: Constant =>
        assignOpUnaryConstant(context)(lvalue,c)(rest)(Li)
      case v @ (_: Identifier | _: Temporary) =>
        assignOpUnary(context)(lvalue,v)(rest)(Move)
      case Additive(op,
        l @ (_: Identifier | _: Temporary | _: Constant),
        r @ (_: Identifier | _: Temporary | _: Constant)
      ) =>
        assignOpBinary(context)(lvalue,l,r)(rest)(additive(op))
      case Equality(op,
        l @ (_: Identifier | _: Temporary | _: Constant),
        r @ (_: Identifier | _: Temporary | _: Constant)
      ) =>
        assignOpBinary(context)(lvalue,l,r)(rest)(equality(op))
      case Relational(op,
        l @ (_: Identifier | _: Temporary | _: Constant),
        r @ (_: Identifier | _: Temporary | _: Constant)
      ) =>
        assignOpBinary(context)(lvalue,l,r)(rest)(relational(op))
      case Multiplicative(op,
        l @ (_: Identifier | _: Temporary | _: Constant),
        r @ (_: Identifier | _: Temporary | _: Constant)
      ) =>
        assignOpBinary(context)(lvalue,l,r)(rest)(multiplicative(op))
      case Unary(op,
        r @ (_: Identifier | _: Temporary)
      ) =>
        assignOpUnary(context)(lvalue,r)(rest)(unary(op))
      case _ =>
        (context, Nil, rest)
    }

  private def assignOpUnaryConstant
    ( context: Context )
    ( lvalue: Identifier | Temporary,
      c: Constant )
    ( rest: Source )
    ( f: (Register, Constant) => TwoAddr
    ): MipsAcc = {
        val (dContext: MipsContext, dest: Register) =
          getRegisterOrTemporary(context,lvalue)
        val statement = f(dest, c)
        (dContext, List(statement), rest)
      }

  private def assignOpUnary
    ( context: Context )
    ( lvalue: Identifier | Temporary,
      rarg: Identifier | Temporary )
    ( rest: Source )
    ( f: (Register, Register) => TwoAddr
    ): MipsAcc = {
        val (dContext: MipsContext, dest: Register) =
          getRegisterOrTemporary(context,lvalue)
        val rreg = getRegisterStrict(dContext,rarg)
        val statement = f(dest,rreg)
        (dContext, List(statement), rest)
      }

  private def assignOpBinary
    ( context: Context )
    ( lvalue: Identifier | Temporary,
      larg: Identifier | Temporary | Constant,
      rarg: Identifier | Temporary | Constant )
    ( rest: Source )
    ( f: (Register, Register, Src) => ThreeAddr
    ): MipsAcc = {
      val (dContext: MipsContext, dest: Register) =
        getRegisterOrTemporary(context,lvalue)
      val (lContext, lreg: Register, code, lRest) =
        getLeftBinaryVar(dContext)(larg)(rest)
      val rreg: Src = rarg match {
        case r @ (_: Identifier | _: Temporary) =>
          getRegisterStrict(lContext, r)
        case c: Constant =>
          c
      }
      var statement: Goal = List(f(dest, lreg, rreg))
      (lContext, statement ++ code, lRest)
    }

  private def getLeftBinaryVar
    ( context: Context )
    ( larg: Identifier | Temporary | Constant)
    ( rest: Source
    ): (Context, Register, Goal, Source) =
      larg match {
        case l @ (_: Identifier | _: Temporary) =>
          val lreg = getRegisterStrict(context,l)
          (context, lreg, Nil, rest)
        case l: Constant =>
          val tempL = Temporary(l)
          val (tempContext,code,tempRest) =
            assignOpUnaryConstant(context)(tempL,l)(rest)(Li)
          val lreg = getRegisterStrict(tempContext,tempL)
          (tempContext, lreg, code, rest)
      }

  private val additive: MipsFor[AdditiveOperators] = {
    case PLUS => Add
    case MINUS => Sub
  }

  private val equality: MipsFor[EqualityOperators] = {
    case EQUAL => Seq
    case NOT_EQUAL => Sne
  }

  private val relational: MipsFor[RelationalOperators] = {
    case LT => Slt
    case GT => Sgt
    case LT_EQ => Sle
    case GT_EQ => Sge
  }

  private val multiplicative: MipsFor[MultiplicativeOperators] = {
    case MULTIPLY => Mul
    case DIVIDE => Div
    case MODULUS => Rem
  }

  private val unary: MipsFor[UnaryOperators] = {
    case NOT => Not
    case NEGATIVE => Neg
  }

  private def foldCode[O]
    ( acc: (Context, Source) )
    ( f: (Context, Source) => MipsAcc )
    ( finisher: (Context, Goal) => O
    ): O = {
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

  private def compose(l: Goal, r: MipsAcc): MipsAcc = {
    var (contextR, codeR, restR) = r
    (contextR, codeR ++ l, restR)
  }
  
  private def getRegisterOrTemporary
    ( context: Context,
      lvalue: Identifier | Temporary
    ): (Context, Register) =
      context.cursor.value(RegisterKey(lvalue))
        .map((context,_))
        .getOrElse {
          assignTemporary(context,lvalue)
        }
  
  private def assignTemporary
    ( context: Context,
      lvalue: Identifier | Temporary
    ): (Context, Register) = {
      val advanced = context.advanceTemporary
      (advanced + (lvalue, advanced.temporary), advanced.temporary)
    }

  private def getRegisterStrict
    ( context: Context,
      lvalue: Identifier | Temporary
    ): Register =
      context.cursor.value(RegisterKey(lvalue))
        .getOrElse {
          val lStr = showLValue(lvalue)
          throw UnexpectedAstNode(s"unknown variable $lStr")
        }

  private def showLValue(lvalue: Identifier | Temporary): String =
    lvalue match {
      case Identifier(id) => id
      case t: Temporary => ("_" + t.hashCode).take(6)
    }
}