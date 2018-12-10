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
import printMips._

object tacToMips extends Stage {
  type Source   = normalToTac.Goal
  type Context  = MipsContext
  type Goal     = List[Assembler]

  private val exitWithArg = Constant(17)
  private val printInt = Constant(1)
  private val readInt = Constant(5)

  private val pseudoMain: Goal = List(
    Label(Scoped(Std.mainIdentifier.id, -1L)),
    Jal(Label(Std.mainIdentifier)),
    Move(A0,V0),
    Li(V0,exitWithArg),
    Syscall,
    Comment("call 17: exit2")
  )

  private def inlinePrintInt
    (context: Context, frame: Frame, value: ASrc): MipsAcc = {
      val post: Goal =
        List(
          Li(V0,printInt),
          Syscall,
          Comment("call 1: print_int")
        )
      evalAssigment(context,frame,A0,value,post.reverse)
    }

  private val inlineReadInt: Goal = List(
    Li(V0,readInt),
    Syscall,
    Comment("call 5: read_int")
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
      private val stack: List[ASrc],
      private val _temporary: Option[Temporaries],
      private val saved: Set[SavedValues],
    ) {
      def advanceTemporary: MipsContext = {
        val temp = _temporary.map { t =>
          if t.enumTag == Temporaries.enumValue.size -1 then {
            throw SemanticError("Too many temporaries!")
          }
          Temporaries.enumValue(t.enumTag + 1)
        } orElse {
          Some(T0)
        }
        copy(_temporary = temp)
      }

      def freeTemporary: MipsContext = {
        val temp = _temporary.map { t =>
          Temporaries.enumValue(t.enumTag - 1)
        } orElse {
          throw SemanticError("No temporary to free!")
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

      def push(asrc: ASrc): MipsContext =
        copy(stack = asrc :: stack)

      def pop: (ASrc, MipsContext) =
        (stack.head, copy(stack = stack.tail))
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
      val mipsContext = MipsContext(cursor,Nil,None,Set())
      goal(mipsContext, tac)
    }

  private def goal
    ( context: Context,
      tac: (DataMap, List[Tac])
    ): (Context, Goal) = {
      val (data, nodes) = tac
      val topLevel = context.cursor.current
      parseMain(topLevel) { () =>
        val (contextFinal, goal) =
          foldCode(topLevelStatements)(context,nodes) {
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
    ( id: Scoped,
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
        case ThreeTac(op, d, l, r) =>
          val (context, dest: Register, post) =
            evalVariableDest(startContext, frame, d)
          val (lContext, lreg: Register, prel) = evalASrcL(context, frame, l)
          val (rContext, rreg: Src, prer) = evalASrcR(lContext, frame, r)
          val result: Assembler = binaryOperators(op)(dest, lreg, rreg)
          val pre = (prer ++ prel): List[Assembler]
          val resultAndStack = result :: pre
          (rContext, post ++ resultAndStack)

        case TwoTac(ASSIGN, d, value) =>
          val (context, dest: Register, post) =
            evalVariableDest(startContext, frame, d)
          evalAssigment(context,frame,dest,value,post)

        case TwoTac(CALL, d, Std.`printIntIdentifier`) =>
          val (context, dest: Register, post) =
            evalVariableDest(startContext, frame, d)
          val (arg: ASrc, nContext) = context.pop
          val (iContext, inlined) = inlinePrintInt(nContext, frame, arg)
          (iContext, post ++ inlined)

        case TwoTac(CALL, d, Std.`readIntIdentifier`) =>
          val (context, dest: Register, post) =
            evalVariableDest(startContext, frame, d)
          val move = Move(dest, V0)
          (context, post ++ (move :: inlineReadInt.reverse))

        case TwoTac(op, d, v) =>
          val (context, dest: Register, post) =
            evalVariableDest(startContext, frame, d)
          val (vContext, rreg: Register, prev) = evalASrcL(context, frame, v)
          val resultAndStack = unary(op)(dest,rreg) :: prev
          (context, post ++ resultAndStack)

        case OneTac(RETURN, value) =>
          evalAssigment(startContext,frame,V0,value,List(Jr(Ra)))

        case OneTac(PUSH_PARAM, a) =>
          (startContext.push(a), Nil)
      }

  private def evalAssigment
    ( context: Context,
      frame: Frame,
      destReg: Register,
      src: ASrc,
      post: List[Assembler]
    ): MipsAcc = {
      src match {
        case c: Constant =>
          (context, post :+ Li(destReg, c))
        case v: Variable =>
          val (vContext, vreg: Register, prev) =
            evalVariableSrc(context, frame, v)
          val ans = Move(destReg, vreg) :: prev
          (vContext, post ++ ans)
      }
    }

  private def evalASrcL
    ( context: Context,
      frame: Frame,
      dest: ASrc
    ): (Context, Register, List[Assembler]) = {
      dest match {
        case c: Constant =>
          val (tContext, treg: Register) =
            assignTemporary(context, new Temporary)
            (tContext, treg, List(Li(treg,c)))
        case v: Variable =>
          evalVariableSrc(context, frame, v)
      }
    }

  private def evalASrcR
    ( context: Context,
      frame: Frame,
      dest: ASrc
    ): (Context, Src, List[Assembler]) = {
      dest match {
        case c: Constant => (context, c, Nil)
        case v: Variable => evalVariableSrc(context, frame, v)
      }
    }

  private def evalVariableSrc
    ( context: Context,
      frame: Frame,
      variable: Variable
    ): (Context, Register, List[Assembler]) = {
      val (vContext, dest: Dest) =
        getRegisterElse(unexpectedInContext)(context,frame,variable)
      dest match {
        case r: Register =>
          (vContext, r, Nil)
        case l: Label =>
          val (tContext, treg: Register) =
            assignTemporary(vContext, new Temporary)
          (tContext, treg, List(Lw(treg,l)))
        case u =>
          throw UnexpectedAstNode(s"Not register or label: $u")
      }
    }
  
  private def evalVariableDest
    ( context: Context,
      frame: Frame,
      variable: Variable
    ): (Context, Register, List[Assembler]) = {
      val (vContext, dest: Dest) =
        getRegisterElse(assignTemporary)(context, frame, variable)
      dest match {
        case r: Register =>
          (vContext, r, Nil: Goal)
        case l: Label =>
          val (tContext, treg: Register) =
            assignTemporary(vContext, new Temporary)
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
          DeclarationKey(i: Scoped), 
          (Declaration(_, _, _: Scoped), _)
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
            case s @ Scoped(i, 0) =>
              frame.globals.get(i).map { _ =>
                (context, Label(s))
              }
            case _ =>
              None
          }
        }
        .getOrElse {
          f(context,lvalue)
        }

  private def assignTemporary
    (context: Context, lvalue: Variable): (Context, Register) = {
      val advanced = context.advanceTemporary
      val added = add(advanced, RegisterKey(lvalue), advanced.temporary)
      (added, advanced.temporary)
    }

  private def unexpectedInContext
    (context: Context, lvalue: Variable): Nothing =
      unexpected(lvalue)

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
      Data :: data
  }
}