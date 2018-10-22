package mycc

import org.graalvm.polyglot._
import mycc.exception._
import mycc.CAst._

object Parser {

  def main(aStrings: Array[String]): Unit = {
    val debug: java.lang.Boolean = aStrings.length != 0 && "-d" == aStrings(0)
    val time = aStrings.length != 0 && "-t" == aStrings(0)
    var old = 0L
    var newtim = 0L
    if (time) {
      old = System.currentTimeMillis
      println("START: N/A")
    }
    MyCCLib.set_debug(debug)
    MyCCLib.init_SymbTable()
    if (time) {
      newtim = System.currentTimeMillis
      println(s"GRAAL_CONTEXT_STARTUP: ${newtim - old}ms")
      old = newtim
    }
    MyCCLib.yyparse()
    val tree: Option[Value] = Option(MyCCLib.get_ans)
    for (t <- tree.filter(!_.isNull)) {
      // MyCCLib.print_ast(t)
      try {
        if (time) {
          newtim = System.currentTimeMillis
          println(s"LEXING_TIME: ${newtim - old}ms")
          old = newtim
        }
        val (context, ast) = parseAst(MyCCLib.astToScala(t))
        if (time) {
          newtim = System.currentTimeMillis
          println(s"IMPORTING_TO_SCALA: ${newtim - old}ms")
          old = newtim
        }
        val (context2, astFlattened) = flattenAst(context, ast)
        if (time) {
          newtim = System.currentTimeMillis
          println(s"CONVERTING_TO_NORMAL: ${newtim - old}ms")
          old = newtim
        }
        println("code:")
        printAst(context2, astFlattened)
        if (time) {
          newtim = System.currentTimeMillis
          println(s"PRINTING_CODE: ${newtim - old}ms")
          old = newtim
        }
        interpretAst(context2, astFlattened)
        if (time) {
          newtim = System.currentTimeMillis
          println(s"INTERPRETING: ${newtim - old}ms")
          old = newtim
        }

      } catch {
        case e: (SemanticError | UnexpectedAstNode | UnimplementedError) =>
          Console.err.println(s"[ERROR] $e")
      }
    }
  }
}