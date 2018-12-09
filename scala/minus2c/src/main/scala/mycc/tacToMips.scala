package mycc

import Ast._
import exception._
import tacToMips._
import normalToTac._
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
import Tac._
import MiscTwoOperators._
import MiscOneOperators._

object tacToMips extends Stage {
  type Source   = normalToTac.Goal
  type Context  = MipsContext
  type Goal     = List[Assembler]

  private val exitWithArg = Constant(17)

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

  private type MipsAcc = (Context, Goal)
  private type Stack = List[Assembler]

  private type BinaryArgs = (Register,Register,Src) => ThreeAddr
  private type UnaryArgs = (Register,Src) => TwoAddr

  private type MipsFor[Op] = Op match {
    case ThreeOperators => PartialFunction[Op,BinaryArgs]
    case TwoOperators => PartialFunction[Op,UnaryArgs]
  }

  case class RegisterKey(key: Variable) extends Bindings.Key {
    type Value = Register
  }

  case class MipsContext
    ( cursor: Cursor,
      private val _temporary: Option[Temporaries],
      private val saved: Set[SavedValues],
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
        copy(_temporary = temp)
      }

      def advanceSaved: (SavedValues, MipsContext) = {
        val savedValues = SavedValues.enumValues.toSet
        val newSet = savedValues &~ saved
        if newSet.isEmpty then {
          throw SemanticError("Too many saved variables!")
        }
        val consumed = newSet.head
        (consumed, copy(saved = saved + consumed))
      }

      def temporary: Temporaries = _temporary.getOrElse { T0 }
    }

  private def nextScope(context: Context): Context =
    context.copy(context.cursor.next)

  private def updateBindings(context: Context, bindings: Bindings): Context =
    context.copy(context.cursor.withBindings(bindings))

  private def add
    (context: Context, key: Bindings.Key, value: key.Value): Context =
      updateBindings(context, context.cursor.current + (key, value))

  def apply
    ( context: Bindings,
      tac: (DataMap, List[Tac])
    ): (Context, Goal) = {
      val cursor = Cursor(context)
      val mipsContext = MipsContext(cursor,None,Set())
      goal(mipsContext, tac)
    }

  private def goal
    ( context: Context,
      tac: (DataMap, List[Tac])
    ): (Context, Goal) = {
      val (data, nodes) = tac
      val topLevel = context.cursor.current
      parseMain(topLevel) { () =>
        val scope = getCurrentScope(context.cursor.current)
        val (uBindings, uTac) =
          renameMainFunc(scope, topLevel, actualMainIdent, nodes)
        val uContext = updateBindings(context, uBindings)
        val (contextFinal, goal) =
          foldCode(topLevelStatements)(uContext,uTac) {
            identity(_,_)
          }
        val dataAssembler = getData(data)
        val top: Goal = predef ++ pseudoMain
        val end: Goal = goal ++ dataAssembler
        (contextFinal, top ++ end)
      }
    }

  private def topLevelStatements
    ( context: Context,
      statement: Tac
    ): MipsAcc =
      statement match {
      case Func(id, frame, body) =>
        evalFunction(nextScope(context))(id, frame, body)
    }

  private def evalFunction
    ( context: Context )
    ( id: Identifier,
      frame: Frame,
      body: List[Code],
    ): MipsAcc =
      foldCode(evalStatements(frame))(defineLocals(context),body) {
        (context,code) => (context, Label(id) :: code.reverse)
      }

  private def evalStatements
    ( frame: Frame )
    ( startContext: Context,
      code: Code
    ): MipsAcc =
      code match {
        case ThreeTac(op, d, c: Constant, r: Variable) =>
          val (context, dest: Register, post) = evalDest(startContext, frame, d)
          val (tContext, treg: Register) =
            assignTemporary(context, new Temporary)
          val loadTemp: Assembler = Li(treg,c)
          val rarg = getRegister(tContext, r)
          val result: Assembler = binaryOperators(op)(dest, treg, rarg)
          (tContext, post ++ List(result, loadTemp)) // code geerated in reverse order
        case ThreeTac(op, d, l: Variable, c: Constant) =>
          val (context, dest: Register, post) = evalDest(startContext, frame, d)
          val lreg = getRegister(context, l)
          val result: Assembler = binaryOperators(op)(dest, lreg, c)
          (context, post ++ List(result))
        case ThreeTac(op, d, l: Variable, r: Variable) =>
          val (context, dest: Register, post) = evalDest(startContext, frame, d)
          val lreg = getRegister(context, l)
          val rarg = getRegister(context, r)
          val result: Assembler = binaryOperators(op)(dest, lreg, rarg)
          (context, post ++ List(result))
        case TwoTac(ASSIGN, d, value) =>
          val (context, dest: Register, post) = evalDest(startContext, frame, d)
          val mips = value match {
            case c: Constant =>
              Li(dest, c)
            case v: Variable =>
              Move(dest, getRegister(startContext, v))
          }
          (context, post ++ List(mips))
        case TwoTac(op, d, v: Variable) =>
          val (context, dest: Register, post) = evalDest(startContext, frame, d)
          val src = getRegister(context,v)
          (context, post ++ List(unary(op)(dest,src)))
        case OneTac(RETURN, value) =>
          val move = value match {
            case c: Constant =>
              Li(V0, c)
            case v: Variable =>
              Move(V0, getRegister(startContext, v))
          }
          (startContext, Jr(Ra) :: move :: Nil)
      }

  private def evalDest
    ( context: Context,
      frame: Frame,
      dest: Variable
    ): (Context, Register, List[Assembler]) = {
      val (topcontext: Context, destination: Dest) =
        getRegisterElse(assignTemporary)(context, frame, dest)
      destination match {
        case r: Register =>
          (topcontext, r, Nil: Goal)
        case l: Label =>
          val (tContext, treg: Register) =
            assignTemporary(topcontext, new Temporary)
          (tContext, treg, List(Sw(treg,l)))
        case _ =>
          throw UnexpectedAstNode("Not register or label")
      }
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

  private val unary: MipsFor[TwoOperators] = {
    case NOT => Not
    case NEGATIVE => Neg
  }

  private def foldCode[O,A]
    (f: (Context, A) => MipsAcc)
    (context: Context, src: List[A])
    (finisher: MipsAcc => O): O = {
      var contextAcc = context
      var rest = src
      var codeAcc: Goal = Nil
      while (rest.nonEmpty) {
        val (context, code) = f(contextAcc, rest.head)
        codeAcc = code ++ codeAcc
        rest = rest.tail
        contextAcc = context
      }
      finisher(contextAcc, codeAcc)
    }

  private def compose(l: Goal, r: MipsAcc): MipsAcc = {
    var (contextR, codeR) = r
    (contextR, codeR ++ l)
  }

  private def defineLocals(context: Context): Context =
    context.cursor.current.topView.foldLeft(context) { (c, kv) =>
      kv match {
        case (
          DeclarationKey(i: Identifier), 
          (Declaration(_, _, _: Identifier), _)
        ) => 
          val (register, advanced) = c.advanceSaved
          add(advanced, RegisterKey(i), register)
        case _ =>
          c
      }
    }

  private def getRegisterElse
    (f: (Context, Variable) => (Context, Dest))
    (context: Context, frame: Frame, lvalue: Variable): (Context, Dest) =
      context.cursor.current
        .genSearch(RegisterKey(lvalue))
        .map((context,_))
        .orElse {
          lvalue match {
            case i: Identifier =>
              frame.globals.get(i).map { _ =>
                (context, Label(i))
              }
            case _ =>
              None
          }
        }
        .getOrElse {
          f(context,lvalue)
        }

  private def getRegister
    (context: Context, lvalue: Variable): Register =
      context.cursor.current
        .genSearch(RegisterKey(lvalue))
        .getOrElse(unexpected(lvalue))
  
  private def assignTemporary
    (context: Context, lvalue: Variable): (Context, Register) = {
      val advanced = context.advanceTemporary
      (add(advanced, RegisterKey(lvalue), advanced.temporary), advanced.temporary)
    }

  private def getData(dataMap: DataMap): Goal = {
    val data =
      dataMap.view.map {
        case (i, c) => List(Label(i), Word(c)): Goal
      }
      .flatMap[Assembler,Iterable[Assembler]](identity)
      .toList
    if data.isEmpty then
      Nil
    else
      Data :: Comment("global data read not yet possible") :: data
  }
}