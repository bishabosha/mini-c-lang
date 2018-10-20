package mycc

import Ast._
import mycc.exception._
import PartialFunctionConversions._
import flattenAst._

object flattenAst extends Stage {
  type Source   = parseCAst.Goal
  type Context  = parseCAst.Context
  type Goal     = List[Statements]

  override def apply(source: Source): (Context, Goal) =
    flattenAst(Bindings.Empty, source)

  def apply(context: Context, source: Source): (Context, Goal) =
    new flattenAst(context).goal(source)

  private def identity[A]: PartialFunction[A, A] = {
    case x => x
  }
}

class flattenAst private (var context: Context) {

  private type Flatten[T] = PartialFunction[T, List[T]]
  private type FlattenO[T, O] = PartialFunction[T, O]

  private def goal: FlattenO[Source, (Context, Goal)] =
    declarationsList ->> { goal => context -> goal }

  private def declarationsList: FlattenO[Source, Goal] = 
    identity ->> { _.flatMap[Statements, List[Statements]](declarations) }

  private def declarations: Flatten[Statements] = 
    function |
    assignments |
    declaration

  private val statementList: FlattenO[List[Statements], List[Statements]] =
    identity ->> { _.flatMap[Statements, List[Statements]](statements) }

  private def statements: Flatten[Statements] =
    block.L |
    assignments |
    declarations |
    jumpStatements

  private def assignments: Flatten[Statements] = assignmentsImpl ->> { _.reverse }

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
      val expanded = v.flatMap[Tac, List[Tac]] { arg =>
        assignmentsImpl(arg)
      }.reverse
      val lVal: Primary = extractVal(expanded.head)
      Return(List(lVal)) :: expanded
  }

  private val assignmentsImpl: FlattenO[Statements, List[Tac]] = {    
    case Assignment(i, v) => extractAssignment(i, v)
    case Equality(op, l, r) => extractBinary(Equality, op, l, r)
    case Relational(op, l, r) => extractBinary(Relational, op, l, r)
    case Additive(op, l, r) => extractBinary(Additive, op, l, r)
    case Multiplicative(op, l, r) => extractBinary(Multiplicative, op, l, r)
    case Unary(op, v) => extractUnary(Unary, op, v)
    case LazyExpressions(e) => e.flatMap[Tac, List[Tac]](assignmentsImpl)
    case Application(p, e) => extractApplication(p, e)
    case p @ (
         (_: Identifier)
       | (_: Constant)
       | (_: StringLiteral)) => Temporary(p) :: Nil
    case t: Temporary => t :: Nil
  }

  private def extractAssignment(i: Identifier, l: Assignments): List[Tac] = {
      val (lVal: Primary, rest) = tryReduce(l)
      new Assignment(i, lVal) :: rest
  }

  private def extractApplication(p: Postfix, e: Expressions) = p match {
      case a: Application => extractApplicationA(a, e)
      case i: Identifier => extractApplicationI(i, e)
      case LazyExpressions(List(i: Identifier)) => extractApplicationI(i, e)
      case LazyExpressions(List(a: Application)) => extractApplicationA(a, e)
      case _ =>
        throw SemanticError("Application of non function type")
    }

  private def extractApplicationA(a2: Application, e: Expressions): List[Tac] = {
    val (args, list) = extractArgs(e)
    val (id: Primary, rest) = tryReduce(a2)
    val end: List[Tac] = list ++ rest
    new Temporary(Application(id, args)) :: end
  }

  private def extractApplicationI(i: Identifier, e: Expressions): List[Tac] = {
    val (args, list) = extractArgs(e)
    new Temporary(Application(i, args)) :: list
  }

  private def extractArgs(e: Expressions): (List[Primary], List[Tac]) = {
    val argc = e.length
    val argExpand = e.map[(Primary, List[Tac]), List[(Primary, List[Tac])]] { arg =>
      tryReduce(arg)
    }
    val args = argExpand.map[Primary, List[Primary]](_._1)
    val list = argExpand.flatMap[Tac, List[Tac]]{_._2}
    (args, list)
  }

  private def extractUnary[Op, A >: Primary](f: (Op, A) => Assignments, op: Op, l: Assignments): List[Tac] = {
      val (lVal: Primary, rest) = tryReduce(l)
      new Temporary(f(op, lVal)) :: rest
  }

  private def extractBinary[Op <: BinaryOp, A >: Primary, B >: Primary](f: (Op, A, B) => Assignments, operand: Op, l: Assignments, r: Assignments): List[Tac] = {
      tryReduce(operand, l, r) {
        val (lVal: Primary, restL) = tryReduce(l)
        val (rVal: Primary, restR) = tryReduce(r)
        val rest: List[Tac] = restL ++ restR
        new Temporary(f(operand, lVal, rVal)) :: rest
      }
  }

  private def tryReduce(operand: BinaryOp, l: Assignments, r: Assignments)(orElse: => List[Tac]): List[Tac] = l match {
    case Constant(lc) => r match {
      case Constant(rc) => Temporary(Constant(operand.op(lc, rc))) :: Nil
      case _ => orElse
    }
    case _ => orElse
  }

  private def tryReduce(assignment: Assignments): (Primary, List[Tac]) = assignment match {
    case primary @ (
      _: Identifier
    | _: Constant
    | _: StringLiteral) =>
      (primary, Nil)
    case x =>
      val rest = assignmentsImpl(x)
      (extractVal(rest.head), rest)
  }

  private def extractVal(tac: Tac): Identifier | Temporary = tac match {
    case t: Temporary => t
    case a: Assignment => a.lvalue
  }
}