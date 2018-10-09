package mycc

object Minus2C {
  import CAst._
  import Ast._
  import Types._

  def specialized(ast: CAst): Ast = ast match {
    case Token("string",   Left(value))       => StringLiteral(value)
    case Token("constant", Right(value))      => Constant(value)
    case Token("id",       Left(identifier))  => Identifier(identifier)
    case Token("int",      _)                 => Type(int)
    case Token("void",     _)                 => Type(void)
    case Token("function", _)                 => Type(void)
    case Token(kind, data)                    => Symbolic(kind, data)
    case Node(kind, left)                     => Node1(kind, specialized(left))
    case BinaryNode(kind, left, right)        => Node2(kind, specialized(left), specialized(right))
  }
}