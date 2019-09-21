package mmc

import Constants._
import exception._
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
import TwoControlOperators._
import OneControlOperators._

object tacToMips extends Stage {
  type Source   = normalToTac.Goal
  type Context  = MipsContext
  type Goal     = List[Assembler]

  private val exitWithArg = IntLiteral(17)
  private val printInt    = IntLiteral(1)
  private val readInt     = IntLiteral(5)

  private val pseudoMain: Goal = List(
    Label(Scoped(Std.mainIdentifier.id, -1L)),
    Jal(Label(Std.mainIdentifier)),
    Move(A0,V0),
    Comment("call 17: exit2"),
    Li(V0,exitWithArg),
    Syscall
  )

  private def doInlinePrintInt(context: Context, value: ASrc): MipsAcc =
    evalAssigment(context, A0, value, inlinePrintInt.reverse)

  private val inlinePrintInt: Goal = List(
    Comment("call 1: print_int"),
    Li(V0,printInt),
    Syscall
  )

  private val inlineReadInt: Goal = List(
    Comment("call 5: read_int"),
    Li(V0,readInt),
    Syscall
  )

  private val predef = List(Globl(Std.mainIdentifier), Text): Goal

  private type MipsAcc = (Context, Goal)
  private type Stack = List[Assembler]

  private type BinaryArgs = (Register,Register,Src) => ThreeAddr
  private type UnaryArgs = (Register,Src) => TwoAddr

  private type MipsFor[Op] = Op match {
    case ThreeOperators => PartialFunction[Op,BinaryArgs]
    case TwoOperators   => PartialFunction[Op,UnaryArgs]
  }

  case class MipsContext
    ( current: Map[Variable, Register],
      frame: Frame,
      private val stack: List[ASrc],
      private val _temporary: Option[Temporaries],
      private val saved: Set[SavedValues],
    ) {
      def advanceTemporary: MipsContext = {
        val temp = _temporary.map { t =>
          if t.ordinal == Temporaries.values.length -1 then {
            throw SemanticError("Too many temporaries!")
          }
          Temporaries.values.find(_.ordinal == t.ordinal + 1).get
        } orElse {
          Some(T0)
        }
        copy(_temporary = temp)
      }

      def freeTemporary: MipsContext = {
        val temp = _temporary.map { t =>
          Temporaries.values.find(_.ordinal == t.ordinal - 1).get
        } orElse {
          throw SemanticError("No temporary to free!")
        }
        copy(_temporary = temp)
      }

      def advanceSaved: (SavedValues, MipsContext) = {
        val savedValues = SavedValues.values.toSet
        val newSet = savedValues &~ saved
        if newSet.isEmpty then {
          throw SemanticError("Too many saved variables!")
        }
        val consumed = newSet.head
        (consumed, copy(saved = saved + consumed))
      }

      def temporary: Temporaries = _temporary.getOrElse { T0 }

      def push(asrc: ASrc): MipsContext = copy(stack = asrc :: stack)

      def pop: (MipsContext, ASrc) = (copy(stack = stack.tail), stack.head)
    }

  private def nextFrame(frame: Frame): Context =
    MipsContext(Map(), frame, Nil, None, Set())

  private def add(context: Context, key: Variable, value: Register): Context =
    context.copy(context.current + (key -> value))

  def apply( context: normalToTac.DataMap, tac: List[Tac]): Goal =
    goal(nextFrame(Frame.Empty), context, tac)

  private def goal(context: Context, data: normalToTac.DataMap, nodes: List[Tac]): Goal = {
    val dataAssembler        = getData(data)
    val (contextFinal, goal) = foldCode(topLevelDeclarations)(context, nodes)(identity)
    val top: Goal = predef ::: pseudoMain
    val end: Goal = goal.reverse ::: dataAssembler
    top ::: end
  }

  private def topLevelDeclarations(context: Context, statement: Tac): MipsAcc = statement match {
    case Func(id, frame, body) => evalFunction(nextFrame(frame))(id, body)
  }

  private def evalFunction(context: Context)(id: Scoped,body: List[Code]): MipsAcc =
    foldCode(evalStatements)(defineLocals(context),body) { (context,code) =>
      (context, code :+ Label(id))
    }

  private def evalStatements(startContext: Context, code: Code): MipsAcc = code match {
    case ThreeTac(op, d, l, r) =>
      val (context, dreg: Register, post)   = evalVariableDest(startContext, d)
      val (lContext, a1reg: Register, prel) = evalASrcL(context, l)
      val (rContext, a2reg: Src, prer)      = evalASrcR(lContext, r)
      val result                            = binaryOperators(op)(dreg, a1reg, a2reg)
      rContext -> (post ::: result :: prer ::: prel)

    case TwoTac(ASSIGN, d, value) =>
      val (context, dest: Register, post) = evalVariableDest(startContext, d)
      evalAssigment(context, dest, value, post)

    case TwoTac(CALL, d, Std.`printIntIdentifier`) =>
      val (context, _, post)    = evalVariableDest(startContext, d)
      val (nContext, arg: ASrc) = context.pop
      val (iContext, inlined)   = doInlinePrintInt(nContext, arg)
      iContext -> (post ::: inlined)

    case TwoTac(CALL, d, Std.`readIntIdentifier`) =>
      val (context, dest: Register, post) = evalVariableDest(startContext, d)
      val move                            = Move(dest, V0)
      context -> (post ::: move :: inlineReadInt.reverse)

    case TwoTac(op, d, v) =>
      val (context, dest: Register, post)   = evalVariableDest(startContext, d)
      val (vContext, a1reg: Register, prev) = evalASrcL(context, v)
      val resultAndStack                    = unary(op)(dest,a1reg) :: prev
      context -> (post ::: resultAndStack)

    case OneTac(RETURN, value) =>
      evalAssigment(startContext, V0, value, Jr(Ra) :: Nil)

    case OneTac(PUSH, a) =>
      startContext.push(a) -> Nil

    case OneControl(JUMP, dest) =>
      startContext -> (J(ControlLabel(dest)) :: Nil)

    case TwoControl(JUMP_IF_ZERO, src, dest) =>
      val (dContext, a1reg: Register, prev) = evalASrcL(startContext, src)
      dContext -> (Beqz(a1reg, ControlLabel(dest)) :: prev)

    case l: LabelIds =>
      startContext -> (ControlLabel(l) :: Nil)
  }

  private def evalAssigment(context: Context, dest: Register, src: ASrc, post: List[Assembler]): MipsAcc = src match {
    case v: Variable =>
      getDest(context, v) match {
        case Some(r: Register) => (context, post :+ Move(dest, r))
        case Some(a: Addresses) => (context, post :+ Lw(dest, a))
        case None => unexpected(v)
      }

    case c: IntLiteral => (context, post :+ Li(dest, c))
  }

  private def evalASrcL(context: Context, dest: ASrc): (Context, Register, List[Assembler]) = dest match {
    case c: IntLiteral =>
      val t = assignTemporary(context, new Temporary)
      t ++ List(Li(t._2,c)) *: ()

    case v: Variable => evalVariableSrc(context, v)
  }

  private def evalASrcR(context: Context, dest: ASrc): (Context, Src, List[Assembler]) = dest match {
    case c: IntLiteral => (context, c, Nil)
    case v: Variable   => evalVariableSrc(context, v)
  }

  private def evalVariableSrc: (Context, Variable) => (Context, Register, List[Assembler]) =
    evalVariable(evalAddressSrc)(unexpectedInContext)

  private def evalVariableDest: (Context, Variable) => (Context, Register, List[Assembler]) =
    evalVariable(evalAddressDest)(assignTemporary)

  private def evalVariable(ifAddress: (Context, Addresses) => (Context, Register, List[Assembler]))(
    ifNone: (Context, Variable) => (Context, Register))(context: Context, variable: Variable)
  : (Context, Register, List[Assembler]) = {
    getDest(context, variable) match {
      case Some(r: Register)  => (context, r, Nil)
      case Some(a: Addresses) => ifAddress(context, a)
      case None               => ifNone(context, variable) ++ Nil *: ()
    }
  }

  private def evalAddressSrc(context: Context, a: Addresses): (Context, Register, List[Assembler]) = {
    val (c, reg: Register) = assignTemporary(context, new Temporary)
    (c, reg, Lw(reg, a) :: Nil)
  }

  private def evalAddressDest(context: Context, a: Addresses): (Context, Register, List[Assembler]) = {
    val (c, reg: Register) = assignTemporary(context, new Temporary)
    (c, reg, Sw(reg, a) :: Nil)
  }

  private def binaryOperators(op: BinaryOp): BinaryArgs = op match {
    case ad: AdditiveOperators       => additive(ad)
    case mu: MultiplicativeOperators => multiplicative(mu)
    case re: RelationalOperators     => relational(re)
    case eq: EqualityOperators       => equality(eq)
  }

  private val additive: MipsFor[AdditiveOperators] = {
    case PLUS  => Add(_,_,_)
    case MINUS => Sub(_,_,_)
  }

  private val equality: MipsFor[EqualityOperators] = {
    case EQUAL     => Seq(_,_,_)
    case NOT_EQUAL => Sne(_,_,_)
  }

  private val relational: MipsFor[RelationalOperators] = {
    case LT    => Slt(_,_,_)
    case GT    => Sgt(_,_,_)
    case LT_EQ => Sle(_,_,_)
    case GT_EQ => Sge(_,_,_)
  }

  private val multiplicative: MipsFor[MultiplicativeOperators] = {
    case MULTIPLY => Mul(_,_,_)
    case DIVIDE   => Div(_,_,_)
    case MODULUS  => Rem(_,_,_)
  }

  private val unary: MipsFor[TwoOperators] = {
    case NOT      => Not(_,_)
    case NEGATIVE => Neg(_,_)
  }

  private def foldCode[O,A](f: (Context, A) => MipsAcc)(context: Context, src: List[A])(finisher: MipsAcc => O): O = {
    var contextAcc = context
    var rest       = src
    var codeAcc    = List.empty[Assembler]
    while rest.nonEmpty do {
      val (context, code) = f(contextAcc, rest.head)
      codeAcc = code ::: codeAcc
      rest = rest.tail
      contextAcc = context
    }
    finisher(contextAcc, codeAcc)
  }

  private def compose(l: Goal, r: MipsAcc): MipsAcc = {
    val (context, codeR) = r
    (context, codeR ::: l)
  }

  private def defineLocals(context: Context): Context =
    context.frame.locals.keys.foldLeft(context) { (c, s) =>
      val (register, advanced) = c.advanceSaved
      add(advanced, s, register)
    }

  private def getDest(context: Context, lvalue: Variable): Option[Dest] = lvalue match {
    case s @ Scoped(i, 0) =>
      context.frame.globals.get(i).map { _ => Label(s) }
    case _ =>
      context.current.get(lvalue)
  }

  private def assignTemporary(context: Context, lvalue: Variable): (Context, Register) = {
    val advanced = context.advanceTemporary
    val added = add(advanced, lvalue, advanced.temporary)
    (added, advanced.temporary)
  }

  private def unexpectedInContext(context: Context, lvalue: Variable): Nothing =
    unexpected(lvalue)

  private def getData(dataMap: normalToTac.DataMap): Goal =
    if dataMap.isEmpty then
      Nil
    else
      Data :: dataMap.flatMap[Assembler]((i, c) => Label(i) :: Word(c) :: Nil).toList
}
