package mmc

import mmclib.{_, given}
import AstInfo._

object DSL {

  object BinaryNode {
    def unapply(node: CAst): Option[(String, CAst, CAst)] = node.asBinaryNode(node.ast.tpe, binary.a1, binary.a2)
  }

  object UnaryNode {
    def unapply(node: CAst): Option[(String, CAst)] = node.asUnaryNode(node.ast.tpe, unary.a1)
  }

  object Singleton {
    def unapply(node: CAst): Option[String] = node.asSingleton(node.ast.tpe)
  }

  object TokenInt {
    def unapply(node: CAst): Option[(String, Int)] = node.asTokenInt(node.ast.tpe, tokenInt.value)
  }

  object TokenString {
    def unapply(node: CAst): Option[(String, String)] = node.asTokenString(node.ast.tpe, tokenString.lexeme)
  }

  object Sequence {
    def unapply(node: CAst): Option[(String, List[CAst])] = {
      val tpe = node.ast.tpe
      node.asSequence(tpe, sequence(tpe, binary.a1, binary.a2))
    }
  }

  private def sequence(tpe: String, a1: CAst, a2: CAst): List[CAst] = {
    var left    = a1
    var right   = a2
    var list    = List.empty[CAst]
    var reverse = false
    var decided = false
    var break   = false
    while (!break) {
      val leftinfo = left.ast
      val rightinfo = right.ast
      if (leftinfo.tag.isInstanceOf[BinaryNode] && leftinfo.tpe == tpe) {
        if (!decided) {
          decided = true;
        }
        list = right :: list
        left.binaryOp {
          left  = binary.a1
          right = binary.a2
        }
      } else if (rightinfo.tag.isInstanceOf[BinaryNode] && rightinfo.tpe == tpe) {
        if (!decided) {
          decided = true
          reverse = true
        }
        list  = left :: list
        right.binaryOp {
          left  = binary.a1
          right = binary.a2
        }
      } else {
        if (reverse) {
          list = right :: left :: list
        } else {
          list = left :: right :: list
        }
        break = true
      }
    }
    if reverse then list.reverse else list
  }

  def (node: CAst) show: String = printAst0(node, 0, StringBuilder()).toString

  private def printAst0(node: CAst, level: Int, builder: StringBuilder): StringBuilder =
    if node.nonEmpty then {
      printLevel(level, builder)
      val ast = node.ast
      node.cata(
        printUnaryNode(level, builder),
        printBinaryNode(level, builder),
        printTokenString(builder),
        printTokenInt(builder),
        printSingleton(builder)
      )
    } else {
      builder
    }

  private def printUnaryNode(level: Int, builder: StringBuilder)(given UnaryNodeOps): StringBuilder = {
    builder.addAll(s"${unary.ast.tpe}\n")
    printAst0(unary.a1, level + 2, builder)
  }

  private def printBinaryNode(level: Int, builder: StringBuilder)(given BinaryNodeOps): StringBuilder = {
    builder.addAll(s"${binary.ast.tpe}\n")
    printAst0(binary.a1, level + 2, builder)
    printAst0(binary.a2, level + 2, builder)
  }

  private def printTokenString(builder: StringBuilder)(given TokenStringOps): StringBuilder = {
    tokenString.ast.tpe match {
      case "string" =>
        builder.addAll("" + '"' + tokenString.lexeme + '"' + '\n')
      case _ =>
        builder.addAll(s"${tokenString.lexeme}\n")
    }
  }

  private def printTokenInt(builder: StringBuilder)(given TokenIntOps): StringBuilder =
    builder.addAll(s"${tokenInt.value}\n")

  private def printSingleton(builder: StringBuilder)(given SingletonOps): StringBuilder =
    builder.addAll(s"${singleton.ast.tpe}\n")

  private def printLevel(level: Int, builder: StringBuilder): StringBuilder = builder.addAll(" " * level)
}
