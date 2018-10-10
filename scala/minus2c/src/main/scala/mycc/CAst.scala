package mycc

enum CAst {
  case Singleton(kind: String)
  case TokenInt(kind: String, data: Int)
  case TokenString(kind: String, data: String)
  case UnaryNode(kind: String, a1: CAst)
  case BinaryNode(kind: String, a1: CAst, a2: CAst)
}

object PrintCAst {
  import CAst._
  def apply(ast: CAst): Unit = printAst(ast, 0)

  private def printAst(ast: CAst, level: Int): Unit = {
    for (_ <- 0 to level - 1) {
      print(' ')
    }
    ast match {
      case UnaryNode(kind, left) => printNode(level, kind, left)
      case BinaryNode(kind, left, right) => printBinaryNode(level, kind, left, right)
      case TokenInt(_, value) => println(value)
      case TokenString(kind, lexeme) => printTokenString(kind, lexeme)
      case Singleton(kind) => println(kind)
    }
  }

  private def printNode(level: Int, kind: String, left: CAst): Unit = {
    println(kind)
    printAst(left, level + 2)
  }

  private def printBinaryNode(level: Int, kind: String, left: CAst, right: CAst): Unit = {
    printNode(level, kind, left)
    printAst(right, level + 2)
  }

  private def printTokenString(kind: String, lexeme: String): Unit =
    if (kind == "string") {
      println(s""""$lexeme"""")
    } else {
      println(lexeme)
    }
}