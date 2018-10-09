package mycc

object SpecialiseCAst {
  import CAst._
  import Ast._
  import Types._

  def apply(ast: CAst): Ast = ast match {
    case Token("string", Left(value)) =>
      StringLiteral(value)

    case Token("constant", Right(value)) =>
      Constant(value)

    case Token("id", Left(identifier)) =>
      Identifier(identifier)

    case Token("int", _) =>
      Type(int)

    case Token("void", _) =>
      Type(void)

    case Token("function", _) =>
      Type(function)

    case Token("ø", _) =>
      ø

    case Token(kind, data) =>
      Symbolic(kind, data)

    case Node(kind, left) =>
      Node1(kind, SpecialiseCAst(left))

    case BinaryNode("if", cond, tail) =>
      tail match {
        case BinaryNode("else", ifTrue, orElse) =>
          IfElse(SpecialiseCAst(cond), SpecialiseCAst(ifTrue), SpecialiseCAst(orElse))
        case _ =>
          If(SpecialiseCAst(cond), SpecialiseCAst(tail))
      }

    case BinaryNode(kind, left, right) =>
      Node2(kind, SpecialiseCAst(left), SpecialiseCAst(right))
  }
}