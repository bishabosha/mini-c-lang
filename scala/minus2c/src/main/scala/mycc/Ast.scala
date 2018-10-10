package mycc

enum Types {
  case int, void, function
}

enum Ast {
  case ø
  case Identifier(id: String)
  case Constant(value: Int)
  case StringLiteral(value: String)
  case Type(id: Types)
  case If(cond: Ast, ifTrue: Ast, orElse: Option[Ast])
  case Return(value: Option[Ast])
  case Leaf(kind: String, data: Either[String, Int])
  case Node0(kind: String)
  case Node1(kind: String, left: Ast)
  case Node2(kind: String, left: Ast, right: Ast)
}