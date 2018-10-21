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

  private def goal: FlattenO[Source, (Context, Goal)] =
    declarationsList ->> { goal => context -> goal }

  private def declarationsList: FlattenO[Source, Goal] = 
    identity ->> { _.flatMap[Statements, List[Statements]](declarations) }

  private def declarations: Flatten[Statements] = 
    function |
    declaration |
    assignments

  private val statementList: FlattenO[List[Statements], List[Statements]] =
    identity ->> { _.flatMap[Statements, List[Statements]](statements) }

  private def statements: Flatten[Statements] =
    block.L |
    declarations |
    assignments |
    jumpStatements

  private def assignments: FlattenO[Statements, Stack] =
    assignmentsImpl ->> { _.reverse }

  private def jumpStatements: Flatten[Statements] =
    jumpStatementsImpl ->> { _.reverse }

  private def function: Flatten[Statements] = {
    case Function(i, body) => Function(i, statementList(body)) :: Nil
  }

  private def declaration: Flatten[Statements] = {
    case d: Declaration => d :: Nil
  }

  private val block: FlattenO[Statements, Block] = {
    case Block(v) => Block(statementList(v))
  }

  private val jumpStatementsImpl: Flatten[Statements] = {
    case Return(v) =>
      extractN(v) { Return(_) }
  }

  private def assignmentsImpl: FlattenO[Statements, Stack] =
    assignmentsRoot |
    constants.E

  private val assignmentsRoot: FlattenO[Statements, Stack] = {    
    case Assignment(i, v) => extractAssignment(i, v)
    case Equality(op, l, r) => extractBinary(Equality.apply, op, l, r)
    case Relational(op, l, r) => extractBinary(Relational.apply, op, l, r)
    case Additive(op, l, r) => extractBinary(Additive.apply, op, l, r)
    case Multiplicative(op, l, r) => extractBinary(Multiplicative.apply, op, l, r)
    case Unary(op, v) => extractUnary(Unary.apply, op, v)
    case LazyExpressions(e) => (expressions ->> { _.reverse })(e)
    case Application(p, e) => extractApplication(p, e)
  }

  private def expressions: FlattenO[Expressions, Stack] =
    identity ->> { _.flatMap[StackVar, Stack](assignmentsImpl) }

  private def extractApplication(p: Postfix, e: Expressions): Stack = p match {
      case a: Application => extractApplicationA(a, e)
      case i: Identifier => extractApplicationP(i, e)
      case LazyExpressions(l) => l.lastOption match {
        case Some(i: Identifier) =>
          extractNC(l) { (args, stack) =>
             extractApplicationP(ex(args.last), e) ++ stack
          }
        case Some(a: Application) =>
          extractNC(l) { (args, stack) =>
            extractApplicationP(ex(args.last), e) ++ stack
          }
        case _ =>
          throw SemanticError("Application of non function type")
      }
      case _ =>
          throw SemanticError("Application of non function type")
    }

  private def extractApplicationA(a2: Application, e: Expressions): Stack = {
    extractNC(e) { (args, stack) =>
      val other = tryReduce(a2)
      other match {
        case (id: StackVar) :: _ =>
          val end: Stack = stack ++ other
          Temporary(Application(ex(id), args)) :: end
        case Nil => stack
      }
    }
  }

  private def extractApplicationP(p: Primary, e: Expressions): Stack = {
    extractNT(e) { Application(p, _) }
  }

  private def extractAssignment(id: Identifier, v: Assignments): Stack = {
    extractN(List(v)) { assignmentArgs(id) }
  }

  private def extractUnary[Op <: UnaryOp, A >: Primary](
    f: (Op, A) => Assignments,
    operand: Op, v: Assignments
  ): Stack = {
    extractNT(List(v)) { unaryArgs(f, operand) }
  }

  private def extractBinary[Op <: BinaryOp, A >: Primary, B >: Primary](
    f: (Op, A, B) => Assignments,
    operand: Op,
    l: Assignments,
    r: Assignments
  ): Stack =
    extractNT(List(l, r)) { binaryArgs(f, operand) }

  private def extractN[O >: StackVar](e: Expressions)(
    f: (List[Assignments]) => O
  ): List[O] = {
    extractNC(e) { (args, stack) => f(args) :: stack }
  }

  private def extractNT(e: Expressions)(
    f: (List[Assignments]) => Assignments
  ): Stack = {
    extractNC(e) { (args, stack) => Temporary(f(args)) :: stack }
  }

  private def assignmentArgs(id: Identifier): PartialFunction[List[Assignments], Assignment] = {
    case (a: Assignments) :: _ => Assignment(id, a)
  }

  private def unaryArgs[Op <: UnaryOp, A >: Primary, O >: Assignments](
    f: (Op, A) => O,
    operand: Op
  ): PartialFunction[List[Assignments], O] = {
    case (a: Assignments) :: _ =>
      a match {
        case c @ Constant(cInner) =>
          Constant(operand.op(cInner))
        case _ =>
          f(operand, arg(a))
      }
  }

  private def binaryArgs[Op <: BinaryOp, A >: Primary, B >: Primary](
    f: (Op, A, B) => Assignments,
    operand: Op
  ): PartialFunction[List[Assignments], Assignments] = {
    case (a: Assignments) :: (b: Assignments) :: _ =>
      a match {
        case lc @ Constant(lcInner) => b match {
          case Constant(rcInner) => Constant(operand.op(lcInner, rcInner))
          case _ => f(operand, lc, arg(b))
        }
        case _ =>
          f(operand, arg(a), arg(b))
      }
  }

  private def extractNC[A](e: Expressions)(f: (List[Assignments], Stack) => A): A = {
    val (args, repush, stack) =
      e.map[Stack, List[Stack]](tryReduce)
       .aggregate { (Nil: List[Assignments], Nil: Stack, Nil: Stack) } (
          (acc, argStack) => {
            val (args, repush, stack) = acc
            argStack match {
              case Temporary(c) :: rest if isPrimary(c) =>
                (args :+ c, repush, stack ++ rest)
              case (a @ Assignment(id, _)) :: rest =>
                (args :+ id, a :: repush, stack ++ rest)
              case s :: rest =>
                (args :+ s, s :: repush, stack ++ rest)
              case Nil =>
                acc
            }
          },
          (acc1, acc2) => {
            val (args1, repush1, stack1) = acc1
            val (args2, repush2, stack2) = acc2
            (args1 ++ args2, repush1 ++ repush2, stack1 ++ stack2)
          }
        )
    f(args, repush ++ stack)
  }

  private def isPrimary(assignments: Assignments): Boolean = assignments match {
    case _: Constant | _: Identifier | _: StringLiteral => true
    case _ => false
  }

  private def canFold(assignments: Assignments): Boolean = assignments match {
    case _: Constant | _: Identifier | _: StringLiteral => true
    case Equality(_, l, r) if isPrimary(l) && isPrimary(r) => true
    case Relational(_, l, r) if isPrimary(l) && isPrimary(r) => true
    case Multiplicative(_, l, r) if isPrimary(l) && isPrimary(r) => true
    case Unary(_, v) if isPrimary(v) => true
    case Application(_, args) if args.forall(a => isPrimary(a) || a.isInstanceOf[Temporary]) => true
    case _ => false
  }

  private def tryReduce: FlattenO[Assignments, Stack] =
    assignmentsRoot |
    (constants ->> Temporary).L

  private def arg[A <: Statements, O >: Primary]: FlattenO[A, O] =
    constants |
     (nodes |
      exAssign)

  private def ex[A <: Statements]: FlattenO[A, Primary] =
    constants |
    exAssign

  private def exAssign[A >: Primary]: FlattenO[A, Primary] = {
    case a: Assignment => a.lvalue
  }

  private def constants[A <: Statements, O >: Primary]: FlattenO[A, O] = {
    case constants @ (
      _: Identifier
    | _: Constant
    | _: StringLiteral
    | _: Temporary
    ) => constants
  }

  private def nodes[A <: Statements, O >: Primary]: FlattenO[A, O] = {
    case constants @ (
      _: Equality
    | _: Relational
    | _: Additive
    | _: Multiplicative
    | _: Unary
    ) => constants.asInstanceOf[O]
  }
}