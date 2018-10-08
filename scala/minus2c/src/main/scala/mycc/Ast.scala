package mycc

enum Ast {
  case Token(kind: String, lexeme: String)
  case Constant(value: Int)
  case Node(kind: String, left: Ast)
  case BinaryNode(kind: String, left: Ast, right: Ast)
}