package mycc

enum Types {
  case int, void, function
}

enum Ast {
  case Identifier(id: String)
  case Constant(value: Int)
  case StringLiteral(value: String)
  case Type(id: Types)
  case Symbolic(kind: String, data: Either[String, Int])
  case Node1(kind: String, left: Ast)
  case Node2(kind: String, left: Ast, right: Ast)
}