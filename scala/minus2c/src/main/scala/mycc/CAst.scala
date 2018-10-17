package mycc

enum CAst {
  case Singleton(kind: String)
  case TokenInt(kind: String, data: Int)
  case TokenString(kind: String, data: String)
  case UnaryNode(kind: String, a1: CAst)
  case BinaryNode(kind: String, a1: CAst, a2: CAst)
}