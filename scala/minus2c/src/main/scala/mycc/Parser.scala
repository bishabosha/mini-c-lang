package mycc

import org.graalvm.polyglot._
import mycc.exception._
import mycc.CAst._

object Parser {
  def main(args: Array[String]): Unit = {
    val debug: java.lang.Boolean = args.contains("-d")
    val time = args.contains("-t")
    val interpret = args.contains("-i")
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
      try {
        if (time) {
          newtim = System.currentTimeMillis
          println(s"LEXING_TIME: ${newtim - old}ms")
          old = newtim
        }
        val (cast, identPool) = MyCCLib.astToScala(t)
        if (time) {
          newtim = System.currentTimeMillis
          println(s"IMPORTING_TO_SCALA: ${newtim - old}ms")
          old = newtim
        }
        val (context, ast) = parseAst(cast, identPool)
        if (time) {
          newtim = System.currentTimeMillis
          println(s"PARSE_CAST: ${newtim - old}ms")
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
        if (interpret) {
          interpretAst(context2, astFlattened)
        }
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