package mycc

import Ast._
import mycc.exception._
import PartialFunctionConversions._
import EqualityOperators._
import flattenAst._

object flattenAst extends Stage {
  type Source   = parseCAst.Goal
  type Context  = parseCAst.Context
  type Goal     = List[Statements]

  private type StackVar = Temporary | Assignment
  private type Stack = List[StackVar]

  override def apply(source: Source): (Context, Goal) =
    flattenAst(Bindings.Empty, source)

  def apply(context: Context, source: Source): (Context, Goal) =
    new flattenAst(context).goal(source)

  private def identity[A]: PartialFunction[A, A] = {
    case x => x
  }
}

class flattenAst private (var context: Context) {

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
    assignmentsRoot |
    constant .E

  private lazy val tryReduce: FlattenO[Assignments, Stack] =
    assignmentsRoot |
    (constant ->> Temporary) .L

  private lazy val jumpStatements: Flatten[Statements] =
    jumpStatementsImpl .R

  private lazy val arg: FlattenO[Assignments, Assignments] =
    node |
    ex

  private lazy val ex: FlattenO[Assignments, Primary] =
    constant |
    assignment ->> { _.lvalue }

  private val statementList: FlattenO[List[Statements], List[Statements]] =
    identity >>- statements

  private lazy val statements: Flatten[Statements] =
    block .L |
    declarations |
    jumpStatements

  private val function: FlattenO[Statements, Function] = {
    case Function(i, body) => Function(i, statementList(body))
  }

  private val declaration: FlattenO[Statements, Declaration] = {
    case d: Declaration => d
  }

  private val block: FlattenO[Statements, Block] = {
    case Block(v) => Block(statementList(v))
  }

  private val jumpStatementsImpl: Flatten[Statements] = {
    case Return(v) => foldExpressionsN(v) { args =>
      Return(args.lastOption.toList)
    }
  }

  private val assignmentsRoot: FlattenO[Statements, Stack] = {
    case Assignment(i, v) => extractAssignment(i, v)
    case Equality(op, l, r) => extractBinary(Equality.apply, op, l, r)
    case Relational(op, l, r) => extractBinary(Relational.apply, op, l, r)
    case Additive(op, l, r) => extractBinary(Additive.apply, op, l, r)
    case Multiplicative(op, l, r) => extractBinary(Multiplicative.apply, op, l, r)
    case Unary(op, v) => extractUnary(Unary.apply, op, v)
    case Application(p, e) => extractApplication(p, e)
    case LazyExpressions(e) => extractExpressions(e)
  }

  private def extractApplication(p: Postfix, e: Expressions): Stack = p match {
      case a: Application =>
        extractApplicationA(a, e)
      case i: Identifier =>
        extractApplicationP(i, e)
      case LazyExpressions(l)
        if l.lastOption.forall(canApply) =>
          extractApplicationE(l, e)
      case _ =>
        throw SemanticError("Application of non function type")
    }

  private val canApply: FlattenO[Assignments, Boolean] = {
    case _: Application | _: Identifier => true
    case _ => false
  }

  private def extractApplicationA(a2: Application, e: Expressions): Stack = {
    extractApplicationE(List(a2), e)
  }

  private def extractApplicationE(lazyExpressions: Expressions, args: Expressions): Stack = {
    foldExpressions(lazyExpressions) { (e, stack) =>
      extractApplicationP(ex(e.last), args) ++ stack
    }
  }

  private def extractApplicationP(p: Primary, e: Expressions): Stack =
    foldExpressionsT(e) { Application(p, _) }

  private def extractAssignment(id: Identifier, v: Assignments): Stack =
    foldExpressions(List(v)) { foldAssignment(id) }

  private def extractExpressions(e: Expressions): Stack =
    foldExpressions(e) { (_, stack) => stack }

  private def foldAssignment(id: Identifier)(
    args: List[Assignments],
    stack: Stack
  ): Stack = args match {
    case (a @ Temporary(t)) :: _ => stack match {
      case (_: Temporary) :: rest => Assignment(id, t) :: rest
      case _ => Assignment(id, a) :: stack
    }
    case (a: Assignments) :: _ => Assignment(id, a) :: stack
    case _ => stack
  }

  private def extractUnary[Op <: UnaryOp, A >: Primary](
    f: (Op, A) => Assignments,
    operand: Op, v: Assignments
  ): Stack = foldExpressionsT(List(v)) { unaryArgs(f, operand) }

  private def extractBinary[Op <: BinaryOp, A >: Primary, B >: Primary](
    f: (Op, A, B) => Assignments,
    operand: Op,
    l: Assignments,
    r: Assignments
  ): Stack = foldExpressionsT(List(l, r)) { binaryArgs(f, operand) }

  private def foldExpressionsN[O >: StackVar](e: Expressions)(
    f: (List[Assignments]) => O
  ): List[O] = foldExpressions(e) { (args, stack) => f(args) :: stack }

  private def foldExpressionsT(e: Expressions)(
    f: (List[Assignments]) => Assignments
  ): Stack = foldExpressions(e) { (args, stack) => Temporary(f(args)) :: stack }

  private def unaryArgs[Op <: UnaryOp, A >: Primary, O >: Assignments](
    f: (Op, A) => O,
    operand: Op
  ): PartialFunction[List[Assignments], O] = {
    case a :: _ =>
      a match {
        case c @ Constant(cInner) =>
          Constant(operand.op(cInner))
        case _ =>
          f(operand, a.asInstanceOf[A])
      }
  }

  private def binaryArgs[Op <: BinaryOp, A >: Primary, B >: Primary](
    f: (Op, A, B) => Assignments,
    operand: Op
  ): PartialFunction[List[Assignments], Assignments] = {
    case a :: b :: _ =>
      a match {
        case lc @ Constant(lcInner) => b match {
          case Constant(rcInner) => Constant(operand.op(lcInner, rcInner))
          case _ => f(operand, lc, b.asInstanceOf[B])
        }
        case _ =>
          f(operand, a.asInstanceOf[A], b.asInstanceOf[B])
      }
  }
  
  private type Acc = (List[Assignments], Stack, Stack)

  private def foldExpressions[A](e: Expressions)(f: (List[Assignments], Stack) => A): A =
    applyArgsAndStack(f) {
      e.map(tryReduce).foldLeft(Nil, Nil, Nil) { extractArgs }
    }

  private def applyArgsAndStack[A](f: (List[Assignments], Stack) => A)(acc: => Acc): A = acc match {
    case (args, repush, stack) => f(args, repush ++ stack)
  }

  private def extractArgs(acc: Acc, argStack: Stack): Acc = acc match {
    case (args, repush, stack) =>
      argStack match {
        case Temporary(c) :: (rest: Stack) if isPrimary(c) =>
          (args :+ c, repush, rest ++ stack)
        case (a @ Assignment(id, _)) :: (rest: Stack) =>
          (args :+ id, a :: repush, rest ++ stack)
        case s :: (rest: Stack) =>
          (args :+ arg(s), s :: repush, rest ++ stack)
        case Nil =>
          acc
      }
  }

  private def isPrimary(assignments: Assignments): Boolean = assignments match {
    case _: Constant | _: Identifier | _: StringLiteral => true
    case _ => false
  }

  private val assignment: FlattenO[Assignments, Assignment] = {
    case a: Assignment => a
  }

  private val constant: FlattenO[Statements, Primary] = {
    case c @ (
      _: Identifier
    | _: Constant
    | _: StringLiteral
    | _: Temporary
    ) => c
  }

  private val node: FlattenO[Statements, Assignments] = {
    case n @ (
      _: Equality
    | _: Relational
    | _: Additive
    | _: Multiplicative
    | _: Unary
    ) => n
  }
}