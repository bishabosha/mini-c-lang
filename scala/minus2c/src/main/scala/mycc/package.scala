package object mycc {
  import CAst._
  import Ast._
  import Types._

  def specialize(ast: CAst): Ast = ast match {
    case Singleton("int") =>
      Type(int)

    case Singleton("void") =>
      Type(void)

    case Singleton("function") =>
      Type(function)

    case Singleton("ø") =>
      ø

    case Singleton("return") =>
      Return(None)

    case Singleton(kind) =>
      Unary(kind)

    case Token("string", Left(value)) =>
      StringLiteral(value)

    case Token("constant", Right(value)) =>
      Constant(value)

    case Token("id", Left(identifier)) =>
      Identifier(identifier)

    case Token(kind, data) =>
      Symbolic(kind, data)

    case Node("return", value) =>
      Return(Some(specialize(value)))

    case Node(kind, left) =>
      Node1(kind, specialize(left))

    case BinaryNode("if", cond, tail) =>
      tail match {
        case BinaryNode("else", ifTrue, orElse) =>
          If(specialize(cond), specialize(ifTrue), Some(specialize(orElse)))
        case _ =>
          If(specialize(cond), specialize(tail), None)
      }

    case BinaryNode(kind, left, right) =>
      Node2(kind, specialize(left), specialize(right))
  }
}