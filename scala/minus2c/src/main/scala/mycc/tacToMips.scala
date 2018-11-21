package mycc

import Ast._
import ArgList._
import exception._
import tacToMips._
import PseudoZero._
import PseudoUnary._
import MIPS._
import Temporaries._
import Misc._
import Arguments._
import Results._
import ZeroAddr._
import OneAddr._
import TwoAddr._
import ThreeAddr._
import AdditiveOperators._
import EqualityOperators._
import RelationalOperators._
import MultiplicativeOperators._
import UnaryOperators._
import StorageTypes._
import Types._
import PartialFunctionConversions._
import scala.collection.mutable

object tacToMips extends Stage {
  type Source   = normalToTac.Goal
  type Context  = MipsContext
  type Goal     = List[Assembler]

  private val exitWithArg = Constant(17)
  private val zero = Constant(0)

  private val actualMainIdent = Identifier("_main_")

  private val pseudoMain: Goal = List(
    Label(Std.mainIdentifier),
    Jal(Label(actualMainIdent)),
    Move(A0,V0),
    Li(V0,exitWithArg),
    Syscall,
    Comment("17: exit with argument"),
  )

  private val predef = List(Globl(Std.mainIdentifier), Text): Goal

  private type MipsAcc = (Source, Context, Goal)
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

      def withCursor(cursor: Cursor): Context =
        MipsContext(stack,cursor,_temporary,saved)
    }

  private def goal
    ( context: Context,
      tac: normalToTac.Goal
    ): (Context, Goal) = {
      val topLevel = context.cursor.current
      local(Std.mainIdentifier, topLevel) match {
        case Some(Std.`mainFunc`) =>
          definition(Std.mainIdentifier,topLevel) match {
            case Some(Function(_,body)) =>
              val newMainDecl = replaceIdent(Std.mainFunc, actualMainIdent)
              val newBindings = rename(
                Std.mainIdentifier,
                actualMainIdent,
                newMainDecl,
                body,
                context.cursor.current
              )
              val newContext =
                context.withCursor(context.cursor.withBindings(newBindings))
              val newCode =
                renameFunction(tac)(actualMainIdent,Std.mainIdentifier)
              val (contextFinal, goal) =
                foldCode(topLevelStatements)(newContext.next.get,newCode)(identity(_,_))
              val top: Goal = predef ++ pseudoMain
              (contextFinal, top ++ goal)
            case _ =>
              throw SemanticError(
                "function definition for `int main(void)` not found.")
          }
        case _ =>
          throw SemanticError(
            "function declaration for `int main(void)` not found.")
      }
    }

  private def topLevelStatements
    ( context: Context,
      tac: Source
    ): MipsAcc = tac match {
      case Function(id, body) :: rest =>
        evalFunction(context,rest)(id, body)
      case _ => (Nil, context, Nil)
    }

  private def renameFunction
    ( tac: Source )
    ( id: Identifier,
      old: Identifier,
    ): Source = tac.foldRight(Nil: Source) { (s,acc) =>
      s match {
        case Function(`old`, body) =>
          Function(id, body) :: acc
        case any => any :: acc
      }
    }

  private def evalFunction
    ( context: Context,
      tac: Source )
    ( id: Identifier,
      body: Source,
    ): MipsAcc =
      foldCode(evalStatements)(defineLocals(context),body) {
        (context,code) => (tac, context, Label(id) :: code.reverse)
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
        rest *:
          assignExpr(getRegisterElse(assignTemporary)(context,id),inner)
      case (temp @ Temporary(inner)) :: rest =>
        rest *:
          assignExpr(getRegisterElse(assignTemporary)(context,temp),inner)
      case Return((expr @ Assign()) :: Nil) :: rest =>
        val (tContext: Context, code: Goal) =
          assignExpr((context, V0),expr)
        rest *: (tContext, Jr(Ra) :: code)
      case _ :: rest => rest *: (context, Nil) // yield rest, consume 1
      case Nil => Nil *: (context, Nil) // yield none
    }

  import AssignmentsPattern._
  private def assignExpr
    ( contextDest: (Context, Register), 
      value: Statements
    ): (Context, Goal) = {
      val (context: Context, dest: Register) = contextDest
      value match {
        case c: Constant =>
          (context,List(Li(dest,c)))
        case v @ RSrc() =>
          val src = getRegister(context,v)
          (context,List(Move(dest,src)))
        case Unary(op, r @ RSrc()) =>
          val src = getRegister(context,r)
          (context,List(unary(op)(dest,src)))
        case Binary(op, c: Constant, r @ RSrc()) =>
          val temp = Temporary(c)
          val (tContext, treg: Register) = assignTemporary(context, temp)
          val loadTemp: Assembler = Li(treg,c)
          val rarg = getRegister(tContext, r)
          val result: Assembler = binaryOperators(op)(dest, treg, rarg)
          (tContext, List(result, loadTemp)) // code geerated in reverse order
        case Binary(op, l @ RSrc(), c: Constant) =>
          val lreg = getRegister(context, l)
          val result: Assembler = binaryOperators(op)(dest, lreg, c)
          (context, List(result))      
        case Binary(op, l @ RSrc(), r @ RSrc()) =>
          val lreg = getRegister(context, l)
          val rarg = getRegister(context, r)
          val result: Assembler = binaryOperators(op)(dest, lreg, rarg)
          (context, List(result))    
        case _ =>
          (context, Nil)
      }
    }

  object Assign {
    def unapply(assign: Assignments): Boolean = true
  }

  object RSrc {
    def unapply(rsrc: Identifier | Temporary): Boolean = true
  }

  object MSrc {
    def unapply(msrc: Identifier | Temporary | Constant): Boolean = true
  }

  private def binaryOperators(op: BinaryOp): BinaryArgs = op match {
    case ad: AdditiveOperators => additive(ad)
    case mu: MultiplicativeOperators => multiplicative(mu)
    case re: RelationalOperators => relational(re)
    case eq: EqualityOperators => equality(eq)
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
    ( f: (Context, Source) => MipsAcc )
    ( context: Context,
      rest: Source )
    ( finisher: (Context, Goal) => O
    ): O = {
      var contextAcc = context
      var restAcc = rest
      var codeAcc: Goal = Nil
      while (!restAcc.isEmpty) {
        val (rest, context, code) = f(contextAcc, restAcc)
        codeAcc = code ++ codeAcc
        restAcc = rest
        contextAcc = context
      }
      finisher(contextAcc, codeAcc)
    }

  private def compose(l: Goal, r: MipsAcc): MipsAcc = {
    var (contextR, restR, codeR) = r
    (contextR, restR, codeR ++ l)
  }

  private def getRegisterElse
    ( f: (Context, Identifier | Temporary) => (Context, Register) )
    ( context: Context,
      lvalue: Identifier | Temporary
    ): (Context, Register) =
      context.cursor.value(RegisterKey(lvalue))
        .map((context,_))
        .getOrElse {
          f(context,lvalue)
        }

  private def getRegister
    ( context: Context,
      lvalue: Identifier | Temporary
    ): Register =
      context.cursor.value(RegisterKey(lvalue)).getOrElse(unexpected(lvalue))
  
  private def assignTemporary
    ( context: Context,
      lvalue: Identifier | Temporary
    ): (Context, Register) = {
      val advanced = context.advanceTemporary
      (advanced + (lvalue, advanced.temporary), advanced.temporary)
    }

  private def unexpected(lvalue: Identifier | Temporary): Nothing = {
    val lStr = showLValue(lvalue)
    throw UnexpectedAstNode(s"unknown variable $lStr")
  }

  private def showLValue(lvalue: Identifier | Temporary): String =
    lvalue match {
      case Identifier(id) => id
      case t: Temporary => ("_" + t.hashCode).take(6)
    }
}