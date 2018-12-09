package mycc

import Ast._
import ArgList._
import exception._
import Temporaries._
import Tac._

object tacToTac extends Stage {
  type Source   = normalToTac.Goal
  type Context  = TacContext
  type Goal     = List[Tac]

  private type TacAcc = (Context, Goal)
  private type Stack = List[Tac]
  private type DataMap = Map[Identifier, GlobalData]

  type RSrc = Identifier | Temporary
  type ASrc = RSrc | Constant

  case class TemporaryKey(key: RSrc) extends Bindings.Key {
    type Value = Temporary
  }

  case class TacContext(cursor: Cursor)

  private def nextScope(context: Context): Context =
    context.copy(context.cursor.next)

  private def updateBindings(context: Context, bindings: Bindings): Context =
    context.copy(context.cursor.withBindings(bindings))

  private def add
    (context: Context, key: Bindings.Key, value: key.Value): Context =
      updateBindings(context, context.cursor.current + (key, value))

  private def get(context: Context, key: Label): Option[Constant] =
    context.cursor.current.genGet(DataKey(key))

  def apply(context: normalToTac.Context, tac: Source): (Context, Goal) = {
    val cursor = Cursor(context)
    val mipsContext = TacContext(cursor)
    goal(mipsContext, tac)
  }

  private def goal
    ( context: Context,
      tac: normalToTac.Goal
    ): (Context, Goal) = {
      val topLevel = context.cursor.current
      parseMain(topLevel) { f =>
        val (contextFinal, goal) =
          foldCode(topLevelStatements)(context,tac) {
            identity(_,_)
          }
        // val data = getData(contextFinal)
        // (contextFinal, goal ++ data)
        (contextFinal, goal)
      }
    }

  private def getData(node: Statements): Option[(Identifier, GlobalData)] = node match {
    case Declaration(_, _, i: Identifier) =>
      Some((i, GlobalWord(i, zero)))
    case Assignment(i: Identifier, c: Constant) =>
      Some((i, GlobalWord(i, c)))
    case _ => None
  }

  private def topLevelStatements
    ( context: Context,
      statement: Statements
    ): TacAcc =
      statement match {
      case Function(id, f, body) =>
        evalFunction(nextScope(context))(id, f, body)
      case _ => (context, Nil)
    }

  private def evalFunction
    ( context: Context )
    ( id: Identifier,
      frame: Frame,
      body: Source,
    ): TacAcc =
      foldCode[Code,TacAcc](evalStatements)(defineLocals(context),body) {
        (context,code) => (context, List(Func(id, frame, code.reverse)))
      }

  private def evalStatements
    ( context: Context,
      statement: Statements
    ): (TacContext, List[Code]) = statement match {
      // case Assignment(id, inner) =>
      //   assignExpr(getRegisterElse(assignTemporary)(context,id),inner)
      // case (temp @ Temporary(inner)) =>
      //   assignExpr(getRegisterElse(assignTemporary)(context,temp),inner)
      // case Return((expr: Assignments) :: Nil) =>
      //   val (tContext: Context, code: Code) =
      //     assignExpr((context, V0),expr)
      //   (tContext, OneTac(MiscOneOperators.RETURN, code))
      case _ => (context, Nil) // yield rest, consume 1
    }

  import AssignmentsPattern._
  private def assignExpr
    (contextDest: (Context, Variable), value: Statements): (Context, List[Code]) = {
      val (topcontext: Context, destination: Variable) = contextDest
      val (context, dest, post) = destination match {
        case r: Identifier =>
          (???, ???, ???)
        case l: Temporary =>
          (???, ???, ???)
        case _ => throw UnexpectedAstNode("Not register or label")
          (???, ???, ???)
      }
      value match {
        case c: Constant =>
          (context, Nil)
        case v: RSrc =>
          (context, Nil)
        case Unary(op, r: RSrc) =>
          (context, Nil)
        case Binary(op, c: Constant, r: RSrc) =>
          (context, Nil)
        case Binary(op, l: RSrc, c: Constant) =>
          (context, Nil)
        case Binary(op, l: RSrc, r: RSrc) =>
          (context, Nil)
        case _ =>
          (context, Nil)
      }
    }

  private def foldCode[A,O]
    (f: (Context, Statements) => (Context,List[A]))
    (context: Context, rest: Source)
    (finisher: (Context, List[A]) => O): O = {
      var contextAcc = context
      var restAcc = rest
      var codeAcc: List[A] = Nil
      while (restAcc.nonEmpty) {
        val (context, code) = f(contextAcc, restAcc.head)
        codeAcc = code ++ codeAcc
        restAcc = restAcc.tail
        contextAcc = context
      }
      finisher(contextAcc, codeAcc)
    }

  private def compose(l: Goal, r: TacAcc): TacAcc = {
    var (contextR, codeR) = r
    (contextR, codeR ++ l)
  }

  private def defineLocals(context: Context): Context =
    context.cursor.current.topView.foldLeft(context) { (c, kv) =>
      kv match {
        case _ =>
          c
      }
    }
}