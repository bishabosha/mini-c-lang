package mycc

import org.graalvm.polyglot._
import mycc.exception._
import mycc.CAst._

object Parser {

  def main(aStrings: Array[String]): Unit = {
    val debug: java.lang.Boolean = aStrings.length != 0 && "-d" == aStrings(0)
    MyCCLib.set_debug(debug)
    MyCCLib.init_SymbTable()
    println("--C COMPILER")
    MyCCLib.yyparse()
    println(s"symbols: ${MyCCLib.get_SymbTable_inst}")
    val tree: Option[Value] = Option(MyCCLib.get_ans)
    val treePointer = tree.map(_.asNativePointer).getOrElse(0L)
    printf("parse finished with 0x%08X\n", treePointer)

    for (t <- tree.filter(!_.isNull)) {
      MyCCLib.print_ast(t)
      try {
        val (context, ast) = parseAst(MyCCLib.astToScala(t))
        println(ast)
        printAst(context, ast)
      } catch {
        case e: (SemanticError | UnexpectedAstNode | UnimplementedError) =>
          Console.err.println(s"[ERROR] $e")
      }
    }
  }
}