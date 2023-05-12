package mmc

import Ast._
import Constants._
import exception._
import PartialFunctionConversions._

object astToNormal extends Stage:
  type Source  = parseCAst.Goal
  type Context = parseCAst.Context
  type Goal    = parseCAst.Goal

  private type Flatten[T] = T =?> List[T]
  private type Stack = List[Assignment]

  def apply(context: Context, source: Source): (Context, Goal) =
    mainDefined(context)(_ -> declarationsList(source))

  private def identity[A]: A =?> A =
    case x => x

  private lazy val declarationsList: Source =?> Goal
    = identity >>- declarations

  private lazy val declarations: Flatten[Declarations]
    = function .L
    | declaration .L
    | assignments

  private lazy val assignments: Statements =?> Stack
    = assignmentsImpl .R

  private lazy val assignmentsImpl: Statements =?> Stack
    = assignmentsWithEffects
    | literals .E

  private lazy val tryReduce: Assignments =?> Stack
    = assignmentsWithEffects
    | (literals ->> temporaryAssignment) .L

  private lazy val jumpStatements: Flatten[Statements]
    = jumpStatementsImpl .R

  private lazy val selectionStatements: Flatten[Statements]
    = selectionStatementsImpl .R

  private lazy val arg: Assignments =?> Assignments
    = node
    | ex

  private lazy val ex: Assignments =?> Primary
    = literals
    | assignment ->> (_.lvalue)

  private lazy val statementList: List[Statements] =?> List[Statements]
    = identity >>- statements

  private lazy val expressions: Expressions =?> Stack
    = expressionsImpl .R

  private lazy val expressionsImpl: Expressions =?> Stack
    = identity >>- (tryReduce .R)

  private lazy val statements: Flatten[Statements]
    = block .L
    | function .L
    | declaration .L
    | assignments
    | jumpStatements
    | selectionStatements

  private val function: Statements =?> Function =
    case Function(i, f, body) => Function(i, f, eliminateTemporaries(statementList(body)))

  private def eliminateTemporaries(statements: List[Statements]): List[Statements] =
    var acc = Nil: List[Statements]
    var left = statements.reverse
    while !left.isEmpty do
      left match
        case (a @ Assignment(id: Scoped, t: Temporary)) :: left1 =>
          left1 match
            case Assignment(`t`, e) :: left2 =>
              acc = Assignment(id, e) :: acc
              left = left2
            case _ =>
              acc = a :: acc
              left = left1
        case a :: left1 =>
          acc = a :: acc
          left = left1
        case _ =>
    acc

  private val declaration: Statements =?> Declaration =
    case d: Declaration => d

  private val block: Statements =?> Block =
    case Block(v) => Block(statementList(v))

  private val jumpStatementsImpl: Flatten[Statements] =
    case Return(v) => foldArguments(v)(mapReturn)

  private def mapReturn(args: Expressions, stack: Stack): List[Statements] =
    args.lastOption match
      case Some(a) => Return(a :: Nil) :: stack
      case _       => Return(Nil)      :: stack

  private val selectionStatementsImpl: Flatten[Statements] =
    case IfElse(ic, test, ifThen, orElse) => foldArguments(test)(mapIfElse(ic, ifThen, orElse))

  private def mapIfElse(ifCount: Long, ifThen: List[Statements], orElse: List[Statements])(
      args: Expressions, stack: Stack): List[Statements] =
    args.lastOption match
      case Some(a) =>
        val ifThenMapped = statementList(ifThen)
        val elseMapped   = statementList(orElse)
        if elseMapped.isEmpty && ifThenMapped.isEmpty then
          stack
        else
          IfElse(ifCount, a :: Nil, ifThenMapped, elseMapped) :: stack

      case _ => throw UnexpectedAstNode("Empty If statement test")

  private val assignmentsWithEffects: Statements =?> Stack =
    case Assignment(d, v) => assignment(d, v)
    case Equality(op, l, r) => binary(Equality.apply, op, l, r)
    case Relational(op, l, r) => binary(Relational.apply, op, l, r)
    case Additive(op, l, r) => binary(Additive.apply, op, l, r)
    case Multiplicative(op, l, r) => binary(Multiplicative.apply, op, l, r)
    case Unary(op, v) => unary(Unary.apply, op, v)
    case Application(p, e) => application(p, e)
    case LazyExpressions(e) => expressions(e)

  private def application(p: Postfix, e: Expressions): Stack = p match
    case a: Application => applicationA(a, e)
    case i: Scoped      => applicationP(i, e)

    case LazyExpressions(l)
      if l.lastOption.forall(canApply) => applicationE(l, e)

    case _ => throw SemanticError("Application of non function type")

  private val canApply: Assignments =?> Boolean =
    case _: (Application | Identifier) => true
    case _ => false

  private def applicationA(a2: Application, e: Expressions): Stack =
    applicationE(List(a2), e)

  private def applicationE(lazyExpressions: Expressions, args: Expressions): Stack =
    foldArguments(lazyExpressions)((e, stack) => applicationP(ex(e.last), args) ::: stack)

  private def applicationP(p: Primary, e: Expressions): Stack =
    foldArgumentsNT(e)(Application(p, _))

  private def assignment(dest: Variable, v: Assignments): Stack =
    foldArguments(List(v))(mapAssignment(dest))

  private def mapAssignment(dest: Variable)(args: Expressions, stack: Stack): Stack = args -> stack match
    case (Assignment(_: Temporary, t) :: _) -> (_ :: rest) => Assignment(dest, t) :: rest
    case (a :: _) -> _                                            => Assignment(dest, a) :: stack
    case _                                                        => stack

  private def unary[Op <: UnaryOp, A >: Primary](f: (Op, A) => Assignments, o: Op, v: Assignments): Stack =
    foldArgumentsNT(List(v))(mapUnary(f, o))

  private def binary[Op <: BinaryOp, A >: Primary, B >: Primary](f: (Op, A, B) => Assignments, o: Op, l: Assignments,
      r: Assignments): Stack =
    foldArgumentsNT(List(l, r))(mapBinary(f, o))

  private def foldArgumentsN(e: Expressions)(f: Expressions => Assignment): Stack =
    foldArguments(e)(f(_)::_)

  private def foldArgumentsNT(e: Expressions)(f: Expressions => Assignments): Stack =
    foldArgumentsN(e)(temporaryAssignment.compose(f))

  private def mapUnary[Op <: UnaryOp, A >: Primary, O >: Assignments](f: (Op, A) => O, o: Op): Expressions =?> O =
    case Constant(a: IntLiteral) :: _ => Constant(IntLiteral(o.op(a.value)))
    case a :: _                       => f(o, a.asInstanceOf[A])

  private def mapBinary[Op <: BinaryOp, A >: Primary, B >: Primary](f: (Op, A, B) => Assignments, o: Op)
    : Expressions =?> Assignments =
    case Constant(a: IntLiteral) :: Constant(b: IntLiteral) :: _ => Constant(IntLiteral(o.op(a.value, b.value)))
    case a :: b :: _                                             => f(o, a.asInstanceOf[A], b.asInstanceOf[B])

  private def foldArguments[A](ex: Expressions)(fn: (Expressions, Stack) => A): A =
    val (args, repush, stack) = ex.map(tryReduce).foldLeft(Nil, Nil, Nil)(mapArguments)
    fn(args, repush ::: stack)

  private type StackAcc = (Expressions, Stack, Stack)

  private def mapArguments(acc: StackAcc, argStack: Stack): StackAcc =
    val (args, repush, stack) = acc
    argStack match
      case Assignment(_: Temporary, c: Literals) :: (rest: Stack) => (args :+ c, repush, rest ::: stack)
      case s :: (rest: Stack)                                     => (args :+ arg(s), s :: repush, rest ::: stack)
      case Nil                                                    => acc

  private val assignment: Assignments =?> Assignment =
    case a: Assignment => a

  private val literals: Statements =?> Primary =
    case c: Literals => c

  private val node: Statements =?> Assignments =
    case n: Node => n
