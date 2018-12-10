package mycc

import Ast._
import mycc.exception._
import PartialFunctionConversions._
import EqualityOperators._
import astToNormal._

object astToNormal extends Stage {
  type Source   = parseCAst.Goal
  type Context  = parseCAst.Context
  type Goal     = List[Statements]

  private type StackVar = Temporary | Assignment
  private type Stack = List[StackVar]

  def apply(context: Context, source: Source): (Context, Goal) =
    new astToNormal(context).goal(source)

  private def identity[A]: PartialFunction[A, A] = {
    case x => x
  }
}

class astToNormal private (var context: Context) {

  private type FlattenO[T, O] = PartialFunction[T, O]
  private type FlattenL[T, O] = FlattenO[T, List[O]]
  private type Flatten[T] = FlattenL[T, T]

  private lazy val goal: FlattenO[Source, (Context, Goal)] =
    declarationsList ->> { goal => context -> goal }

  private lazy val declarationsList: FlattenO[Source, Goal] = 
    identity >>- declarations

  private lazy val declarations: Flatten[Statements] = 
    function .L |
    declaration .L |
    assignments

  private lazy val assignments: FlattenO[Statements, Stack] =
    assignmentsImpl .R

  private lazy val assignmentsImpl: FlattenO[Statements, Stack] =
    assignmentsWithEffects |
    constant .E

  private lazy val tryReduce: FlattenO[Assignments, Stack] =
    assignmentsWithEffects |
    (constant ->> temporaryAssignment) .L

  private lazy val jumpStatements: Flatten[Statements] =
    jumpStatementsImpl .R

  private lazy val arg: FlattenO[Assignments, Assignments] =
    node |
    ex

  private lazy val ex: FlattenO[Assignments, Primary] =
    constant |
    assignment ->> { _.lvalue }

  private lazy val statementList: FlattenO[List[Statements], List[Statements]] =
    identity >>- statements

  private lazy val expressions: FlattenO[Expressions, Stack] =
    expressionsImpl .R

  private lazy val expressionsImpl: FlattenO[Expressions, Stack] =
    identity >>- (tryReduce .R)

  private lazy val statements: Flatten[Statements] =
    block .L |
    declarations |
    jumpStatements

  private val function: FlattenO[Statements, Function] = {
    case Function(i, f, body) => Function(i, f, statementList(body))
  }

  private val declaration: FlattenO[Statements, Declaration] = {
    case d: Declaration => d
  }

  private val block: FlattenO[Statements, Block] = {
    case Block(v) => Block(statementList(v))
  }

  private val jumpStatementsImpl: Flatten[Statements] = {
    case Return(v) => foldArguments(v) { mapReturn }
  }

  private def mapReturn
    ( args: List[Assignments],
      stack: Stack
    ): List[Statements] =
      (args.lastOption, stack) match {
        case (Some(a), _) =>
          Return(a :: Nil) :: stack
        case _ =>
          Return(Nil) :: stack
      }

  private val assignmentsWithEffects: FlattenO[Statements, Stack] = {
    case Assignment(d, v) => assignment(d, v)
    case Equality(op, l, r) => binary(Equality, op, l, r)
    case Relational(op, l, r) => binary(Relational, op, l, r)
    case Additive(op, l, r) => binary(Additive, op, l, r)
    case Multiplicative(op, l, r) => binary(Multiplicative, op, l, r)
    case Unary(op, v) => unary(Unary, op, v)
    case Application(p, e) => application(p, e)
    case LazyExpressions(e) => expressions(e)
  }

  private def application(p: Postfix, e: Expressions): Stack = p match {
      case a: Application =>
        applicationA(a, e)
      case i: Identifier =>
        applicationP(i, e)
      case LazyExpressions(l)
        if l.lastOption.forall(canApply) =>
          applicationE(l, e)
      case _ =>
        throw SemanticError("Application of non function type")
    }

  private val canApply: FlattenO[Assignments, Boolean] = {
    case _: (Application | Identifier) => true
    case _ => false
  }

  private def applicationA(a2: Application, e: Expressions): Stack = {
    applicationE(List(a2), e)
  }

  private def applicationE(lazyExpressions: Expressions, args: Expressions): Stack = {
    foldArguments(lazyExpressions) { (e, stack) =>
      applicationP(ex(e.last), args) ++ stack
    }
  }

  private def applicationP(p: Primary, e: Expressions): Stack =
    foldArgumentsNT(e) { Application(p, _) }

  private def assignment(dest: Variable, v: Assignments): Stack =
    foldArguments(List(v)) { mapAssignment(dest) }

  private def mapAssignment
    ( dest: Variable )
    ( args: List[Assignments],
      stack: Stack
    ): Stack =
      (args, stack) match {
        case (Assignment(_: Temporary, t) :: _, _ :: (rest: Stack)) =>
          Assignment(dest, t) :: rest
        case (a :: _, _) =>
          Assignment(dest, a) :: stack
        case _ =>
          stack
      }

  private def unary[Op <: UnaryOp, A >: Primary]
    ( f: (Op, A) => Assignments,
      o: Op, v: Assignments
    ): Stack =
      foldArgumentsNT(List(v)) { mapUnary(f, o) }

  private def binary[Op <: BinaryOp, A >: Primary, B >: Primary]
    ( f: (Op, A, B) => Assignments,
      o: Op,
      l: Assignments,
      r: Assignments
    ): Stack =
      foldArgumentsNT(List(l, r)) { mapBinary(f, o) }

  private def foldArgumentsN[O >: StackVar]
    ( e: Expressions )
    ( f: (List[Assignments] ) => O
    ): List[O] =
      foldArguments(e) { f(_) :: _ }

  private def foldArgumentsNT
    ( e: Expressions )
    ( f: (List[Assignments]) => Assignments
    ): Stack =
      foldArgumentsN(e) { temporaryAssignment.compose(f) }

  private def mapUnary[Op <: UnaryOp, A >: Primary, O >: Assignments]
    ( f: (Op, A) => O,
      o: Op
    ): PartialFunction[List[Assignments], O] = {
      case Constant(a) :: _ =>
        Constant(o.op(a))
      case a :: _ =>
        f(o, a.asInstanceOf[A])
    }

  private def mapBinary[Op <: BinaryOp, A >: Primary, B >: Primary]
    ( f: (Op, A, B) => Assignments,
      o: Op
    ): PartialFunction[List[Assignments], Assignments] = {
      case Constant(a) :: Constant(b) :: _ =>
        Constant(o.op(a, b))
      case a :: b :: _ =>
        f(o, a.asInstanceOf[A], b.asInstanceOf[B])
    }
  

  private def foldArguments[A]
    ( ex: Expressions )
    ( fn: (List[Assignments], Stack) => A
    ): A = {
      val (args, repush, stack) =
        ex.map(tryReduce)
          .foldLeft(Nil, Nil, Nil) { mapArguments }
      fn(args, repush ++ stack)
    }

  private type StackAcc = (List[Assignments], Stack, Stack)

  private def mapArguments(acc: StackAcc, argStack: Stack): StackAcc =
    acc match {
      case (args, repush, stack) =>
        argStack match {
          case Assignment(_: Temporary, c: Constants) :: (rest: Stack) =>
            (args :+ c, repush, rest ++ stack)
          case s :: (rest: Stack) =>
            (args :+ arg(s), s :: repush, rest ++ stack)
          case Nil =>
            acc
        }
    }

  private val assignment: FlattenO[Assignments, Assignment] = {
    case a: Assignment => a
  }

  private val constant: FlattenO[Statements, Primary] = {
    case c: (Constants | Temporary) => c
  }

  private val node: FlattenO[Statements, Assignments] = {
    case n: Node => n
  }
}