package mycc

enum CAst {
  case Singleton(kind: String)
  case Token(kind: String, data: Either[String, Int])
  case UnaryNode(kind: String, left: CAst)
  case BinaryNode(kind: String, left: CAst, right: CAst)
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
      case Token(kind, data) => printToken(kind, data)
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

  private def printToken(kind: String, data: Either[String, Int]): Unit = data match {
    case Left(intval) =>
      println(intval)
    case Right(lexeme) =>
      if (kind == "string") {
        println(s""""$lexeme"""")
      } else {
        println(lexeme)
      }
  }
}