package mmc

import Ast._
import Constants._
import exception._
import Tac._
import MiscTwoOperators._
import MiscOneOperators._
import TwoControlOperators._
import OneControlOperators._

object normalToTac extends Stage:
  type Source  = astToNormal.Goal
  type Context = DataMap
  type Goal    = List[Tac]

  type DataMap = Map[Scoped, Global]

  object ASrc:
    def unapply(ast: Variable | Constant): Option[ASrc] = ast match
      case Constant(i: IntLiteral) => Some(i)
      case v: Variable             => Some(v)
      case _                       => None

  object DataMap:
    val empty: DataMap = Map.empty

  def apply(nodes: List[Declarations]): (Context, Goal) =
    val data = nodes.foldLeft(DataMap.empty)(
      (acc, declaration) =>
        getData(declaration).fold(acc)(acc + _)
    )
    val code = nodes.foldRight(List.empty[Tac])(
      topLevelDeclaration(_) ::: _
    )
    (data, code)

  private def getData(node: Declarations): Option[(Scoped, Global)] = node match
    case Declaration(_, _, i: Scoped)         => Some((i, zero))
    case Assignment(i: Scoped, c: IntLiteral) => Some((i, c))
    case _                                    => None

  private def topLevelDeclaration
    (node: Declarations): List[Tac] =
      node match
        case Function(s, f, body) =>
          val validated = body.foldLeft(List.empty[Code])((code, statement) =>
            evalStatement(statement) ::: code
          )
          List(Func(s, f, eliminateJumps(validated)))
        case _ => Nil

  private def evalStatement(node: Statements): List[Code] =
    node match
      case Assignment(dest, expr: ExpressionRoot) =>
        evalExpr(dest,expr)
      case Assignment(dest, Application(id @ Scoped(_, s), args)) =>
        if s == 0 then
          evalApplication(dest, id, args)
        else
          throw SemanticError("Can only call global functions")
      case Return(ASrc(a) :: Nil) =>
        List(OneTac(RETURN, a))
      case Block(nodes) => nodes.foldLeft(List.empty)((code, statement) =>
        evalStatement(statement) ::: code
      )
      case IfElse(ifCount, ASrc(isOne) :: Nil, ifTrue, elsePart) =>
        val joinLabel = Join(ifCount)
        val elseLabel: Option[LabelIds] = elsePart.headOption.map(_ => ElseLabel(ifCount))
        val ifZero: LabelIds = elseLabel.getOrElse(joinLabel)
        val jumpIfZero = TwoControl(JUMP_IF_ZERO, isOne, ifZero)
        val joinCommand = OneControl(JUMP, joinLabel)
        val ifOne = ifTrue.foldLeft(List.empty[Code])((code, statement) =>
          evalStatement(statement) ::: code
        )
        val orElse = elsePart.foldLeft(List.empty[Code])((code, statement) =>
          evalStatement(statement) ::: code
        )
        val code: Option[List[Code]] = for elseL <- elseLabel yield
          val endIf: List[Code]   = (joinCommand :: ifOne).reverse
          val endElse: List[Code] = elseL :: (joinCommand :: orElse).reverse
          val both: List[Code]    = endIf ::: endElse
          (jumpIfZero :: both) :+ joinLabel
        val finalCode: List[Code] = code getOrElse:
          val endIf: List[Code]   = (joinCommand :: ifOne).reverse
          (jumpIfZero :: endIf) :+ joinLabel
        finalCode.reverse
      case _ =>
        List()

  private def eliminateJumps(code: List[Code]): List[Code] =
    var acc = Nil: List[Code]
    var left = code
    while !left.isEmpty do
      left match
        case (l: LabelIds) :: left1 =>
          left1 match
            case OneControl(JUMP, `l`) :: left2 =>
              acc = l :: acc
              left = left2
            case _ =>
              acc = l :: acc
              left = left1
        case a :: left1 =>
          acc = a :: acc
          left = left1
        case _ =>
    acc

  private def evalApplication
    (dest: Variable, id: Scoped, args: Expressions): List[Code] =
      args.foldRight(List[Code](TwoTac(CALL, dest, id))) :
        (exp, acc) =>
          exp match
          case ASrc(a) =>
            OneTac(PUSH, a) :: acc
          case _ =>
            acc
      .reverse

  private def evalExpr(dest: Variable, expr: ExpressionRoot): List[Code] = expr match
    case Multiplicative(op, ASrc(l), ASrc(r)) => ThreeTac(op, dest, l, r) :: Nil
    case Additive(op, ASrc(l), ASrc(r))       => ThreeTac(op, dest, l, r) :: Nil
    case Relational(op, ASrc(l), ASrc(r))     => ThreeTac(op, dest, l, r) :: Nil
    case Equality(op, ASrc(l), ASrc(r))       => ThreeTac(op, dest, l, r) :: Nil
    case Unary(op, ASrc(v))                   => TwoTac(op, dest, v)      :: Nil
    case ASrc(b)                              => TwoTac(ASSIGN, dest, b)  :: Nil
    case _                                    => Nil
