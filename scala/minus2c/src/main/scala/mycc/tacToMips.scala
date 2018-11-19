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

  private val predef = List(Globl(main), Text): Goal

  private type MipsAcc = (Context, Goal, Source)
  private type Stack = List[Assembler]

  private type MipsFor[Op] =
    PartialFunction[Op,(Register,Register,Register | Constant) => ThreeAddr]

  private type MipsForUnary[Op] =
    PartialFunction[Op,(Register,Register | Constant) => TwoAddr]

  def apply(context: normalToTac.Context, tac: Source): (Context, Goal) =
    goal(MipsContext(Nil, Cursor(Nil, Map(), context), None, Set()), tac)

  case class MipsContext
    (
      stack: List[MipsContext],
      cursor: Cursor[Register | Goal],
      private val _temporary: Option[Temporaries],
      private val saved: Set[SavedValues]
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
        MipsContext(stack, cursor, temp, saved)
      }

      def advanceSaved: (SavedValues, MipsContext) = {
        val savedValues = SavedValues.enumValues.toSet
        val newSet = savedValues &~ saved
        if (newSet.isEmpty) {
          throw SemanticError("Too many saved variables!")
        }
        val consumed = newSet.head
        (consumed, MipsContext(stack, cursor, _temporary, saved + consumed))
      }

      def temporary: Temporaries = _temporary.getOrElse { T0 }

      def +(pair: (Key | Temporary, Register | Goal)): MipsContext =
        MipsContext(stack, cursor + pair, _temporary, saved)

      def next: Option[MipsContext] = cursor.next.map {
        MipsContext(this :: stack, _, None, Set())
      }
    }

  private def goal
    ( context: Context,
      tac: normalToTac.Goal
    ): (Context, Goal) = {
      val topLevel = context.cursor.current
      topLevel.local(main) match {
        case Some(`mainFunc`)
          if topLevel.definition(main).isDefined =>
            foldCode(context.next.get, tac)(topLevelStatements)((a,b)=>(a,b))
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
      foldCode(defineLocals(context), body)(evalStatements) { (context,code) =>
        (context, Label(id) :: code.reverse, rest)
      }

  private def defineLocals(context: Context): Context =
    context.cursor.current.seen.foldLeft(context) { (c, kv) =>
      kv match {
        case (i: Identifier, Declaration(_, _, _: Identifier)) => 
          val (register, advanced) = c.advanceSaved
          advanced + (i -> register)
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
        assignConstant(context, lvalue, c, rest)
      case v @ (_: Identifier | _: Temporary) =>
        assignVariable(context, lvalue, v, rest)
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

  private def assignConstant
    ( context: Context,
      lvalue: Identifier | Temporary,
      c: Constant,
      rest: Source
    ): MipsAcc =
      getRegisterOrAssignTemporary(context,lvalue) { (context, dest) =>
        val statement = Li(dest, c)
        (context + (lvalue -> dest), List(statement), rest)
      }

  private def assignVariable
    ( context: Context,
      lvalue: Identifier | Temporary,
      rvalue: Identifier | Temporary,
      rest: Source
    ): MipsAcc =
      getRegisterOrAssignTemporary(context,lvalue) { (context, dest) =>
        getRegisterStrict(context,rvalue) { (context, source) =>
          val statement = Move(dest, source)
          (context + (lvalue -> dest), List(statement), rest)
        }
      }

  private def assignOpUnary[O]
    ( context: Context )
    ( lvalue: Identifier | Temporary,
      rarg: Identifier | Temporary )
    ( rest: Source )
    ( f: (Register, Register | Constant) => TwoAddr
    ): MipsAcc = 
      getRegisterOrAssignTemporary(context,lvalue) { (context, dest) =>
        getRegisterStrict(context + (lvalue -> dest),rarg) { (context, rreg) =>
          performOpUnary(context)(dest,rreg)(rest)(f)
        }
      }

  private def assignOpBinary[O]
    ( context: Context )
    ( lvalue: Identifier | Temporary,
      larg: Identifier | Temporary | Constant,
      rarg: Identifier | Temporary | Constant )
    ( rest: Source )
    ( f: (Register,Register,Register | Constant) => ThreeAddr
    ): MipsAcc = (larg, rarg) match {
      case (
        l @ (_: Identifier | _: Temporary),
        r @ (_: Identifier | _: Temporary)
      ) =>
        getRegisters(context,lvalue,l,r){(context, dest, lreg, rreg) =>
          performOpBinary(context)(dest,lreg,rreg)(rest)(f)
        }
      case (
        l @ (_: Identifier | _: Temporary),
        r: Constant
      ) =>
        getRegisters(context,lvalue,l){(context, dest, lreg) =>
          performOpBinary(context)(dest,lreg,r)(rest)(f)
        }
      case (
        l: Constant,
        r @ (_: Identifier | _: Temporary)
      ) =>
        val tempL = Temporary(l)
        val (tempContext,code,tempRest) = assignConstant(context,tempL,l,rest)
        val opMips = getRegisters(tempContext,lvalue,tempL,r) {
          (context, dest, lreg, rreg) =>
            performOpBinary(context)(dest,lreg,rreg)(tempRest)(f)
          }
        compose(code,opMips)
      case _ =>
        (context, Nil, rest)
    }

  private def performOpUnary[O]
    ( context: Context )
    ( dest: Register,
      rsrc: Register | Constant )
    ( rest: Source )
    ( f: (Register,Register | Constant) => TwoAddr
    ): MipsAcc = {
      val statement = f(dest,rsrc)
      (context, List(statement), rest)
    }

  private def performOpBinary[O]
    ( context: Context )
    ( dest: Register,
      lreg: Register,
      rsrc: Register | Constant )
    ( rest: Source )
    ( f: (Register,Register,Register | Constant) => ThreeAddr
    ): MipsAcc = {
      val statement = f(dest,lreg,rsrc)
      (context, List(statement), rest)
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

  private val unary: MipsForUnary[UnaryOperators] = {
    case NOT => Not
    case NEGATIVE => Neg
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

  private def compose(l: Goal, r: MipsAcc): MipsAcc = {
    var (contextR, codeR, restR) = r
    (contextR, codeR ++ l, restR)
  }

  private def asRegister[O]
    ( node: Register | Goal )
    ( f: Register => O
    ): Option[O] = {
      node match {
        case v @ (
          _: Results
        | _: Arguments
        | _: Temporaries
        | _: SavedValues
        | _: Trap
        | _: Misc
        ) =>
          Some(f(v))
        case _ =>
          None
      }
    }
  
  private def getRegisterOrAssignTemporary
    ( context: Context,
      lvalue: Identifier | Temporary )
    ( f: (Context, Register) => MipsAcc
    ): MipsAcc =
      getRegister(context,lvalue)(f).getOrElse {
        assignTemporary(context)(f)
      }
  
  private def assignTemporary
    ( context: Context)
    ( f: (Context, Register) => MipsAcc
    ): MipsAcc = {
      val advanced = context.advanceTemporary
      f(advanced, advanced.temporary)
    }

  private def getRegisters
    ( context: Context,
      lvalue: Identifier | Temporary,
      rvalue: Identifier | Temporary )
    ( f: (Context, Register, Register) => MipsAcc
    ): MipsAcc =
      getRegisterStrict(context,lvalue) { (context, lreg) =>
        getRegisterStrict(context, rvalue) { (context, rreg) =>
          f(context,lreg,rreg)
        }
      }

  private def getRegisters
    ( context: Context,
      value1: Identifier | Temporary,
      value2: Identifier | Temporary,
      value3: Identifier | Temporary )
    ( f: (Context, Register, Register, Register) => MipsAcc
    ): MipsAcc =
      getRegisterStrict(context,value1) { (context, reg1) =>
        getRegisterStrict(context,value2) { (context, reg2) =>
          getRegisterStrict(context,value3) { (context, reg3) =>
            f(context,reg1,reg2,reg3)
          }
        }
      }

  private def getRegisterStrict
    ( context: Context,
      lvalue: Identifier | Temporary )
    ( f: (Context, Register) => MipsAcc
    ): MipsAcc =
      getRegister(context,lvalue)(f).getOrElse {
        throw UnexpectedAstNode(s"unknown variable ${showLValue(lvalue)}")
      }

  private def getRegister
    ( context: Context,
      lvalue: Identifier | Temporary )
    ( f: (Context, Register) => MipsAcc
    ): Option[MipsAcc] =
      context.cursor.value(lvalue).flatMap {
        asRegister(_) { reg =>
          f(context, reg)
        }
      }

  private def showLValue(lvalue: Identifier | Temporary): String =
    lvalue match {
      case Identifier(id) => id
      case t: Temporary => ("_" + t.hashCode).take(6)
    }
}