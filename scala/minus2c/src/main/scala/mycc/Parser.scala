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
    val tree: Option[Value] = Option(MyCCLib.get_ans)

    for (t <- tree.filter(!_.isNull)) {
      // MyCCLib.print_ast(t)
      try {
        val (context, ast) = parseAst(MyCCLib.astToScala(t))
        // println(ast)
        val (context2, astFlattened) = flattenAst(context, ast)
        println("CODE:")
        printAst(context2, astFlattened)
      } catch {
        case e: (SemanticError | UnexpectedAstNode | UnimplementedError) =>
          Console.err.println(s"[ERROR] $e")
      }
    }
  }
}