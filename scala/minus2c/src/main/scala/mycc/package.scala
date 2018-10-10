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
      Node0(kind)

    case TokenString("string", value) =>
      StringLiteral(value)

    case TokenString("id", identifier) =>
      Identifier(identifier)

    case TokenString(kind, data) =>
      Leaf(kind, Left(data))

    case TokenInt("constant", value) =>
      Constant(value)

    case TokenInt(kind, data) =>
      Leaf(kind, Right(data))

    case UnaryNode("return", value) =>
      Return(Some(specialize(value)))

    case UnaryNode(kind, left) =>
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