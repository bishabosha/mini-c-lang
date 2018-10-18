package mycc

import Ast._
import StorageTypes._
import Types._
import ArgList._
import parseCAst._
import exception.SemanticError

object printAst {

  def apply(context: Context, nodes: List[Ast]): Unit = printAst0(context, nodes, 0)

  private def printAst0(context: Context, nodes: List[Ast], level: Int): Unit = {
    for (node <- nodes) {
      printAstNode(context, node, level)
    }
  }

  private def printAstNode(context: Context, node: Ast, level: Int): Unit = node match {
    case Declaration(storage, types, declarator) =>
      declarator match {
        case Identifier(id) => printLeveln(s"${storageToString(storage)}$types $id;", level)
        case FunctionDeclarator(Identifier(id), args) =>
          printLeveln(s"${storageToString(storage)}$types $id${getArgList(args)};", level)
      }
    case Assignment(Identifier(id), value) =>
      printLeveln(s"$id = ???;", level)
    case Function(i @ Identifier(id), b) =>
      for (Declaration(storage, types, FunctionDeclarator(_, args)) <- context.scope(i)) {
        printFn(context, storage, types, id, args, b, level)
      }
    case b: Block => printBlock(context, b, level)
    case _ => printLeveln("???", level)
  }

  private def getArgList(a: ArgList): String = a match {
    case LVoid => "(void)"
    case LAny => "()"
    case LParam(params) =>
      params.view.map {
        case t: Types => s"$t"
        case (t: Types, _ @ Identifier(i)) => s"$t $i"
      }.mkString("(", ", ", ")")
  }

  private def printFn(context: Context, storage: StorageTypes, types: Types, name: String, args: ArgList, b: List[Statements], level: Int): Unit = {
    printLeveln(s"${storageToString(storage)}$types $name${getArgList(args)} {", level)
    if (!b.isEmpty) {
      printAst0(context, b, inc(level))
    }
    printLeveln("}", level)
  }

  private def printBlock(context: Context, b: Block, level: Int): Unit = {
    printLevel("{", level)
    if (b.inner.isEmpty) {
      println("}")
    } else {
      println
      printAst0(context, b.inner, inc(level))
      printLeveln("}", level)
    }
  }

  private def inc(level: Int) = level + 2

  private def storageToString(s: StorageTypes) = s match {
    case StorageTypes.auto => ""
    case _ => s"$s "
  }

  private def printLeveln(s: String, l: Int): Unit = printLevel(s + "\n", l)

  private def printLevel(s: String, l: Int): Unit = {
    for (_ <- 0 to l - 1) {
      print(" ")
    }
    print(s)
  }
}