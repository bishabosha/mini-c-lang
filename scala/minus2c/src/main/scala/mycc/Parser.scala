package mycc

import org.graalvm.polyglot._
import mycc.exception._
import mycc.CAst._

object Parser {
  def main(args: Array[String]): Unit = {
    val debug: java.lang.Boolean = args.contains("-d")
    val doTime = args.contains("-time")
    val doInterpret = args.contains("-i")
    val doPrintNormal = args.contains("-n")
    val doPrintMips = args.contains("-m")
    val doPrintTac = args.contains("-t")
    val doSeparate = args.contains("-sep")
    var old = 0L
    var newtim = 0L
    if doTime then {
      old = System.currentTimeMillis
      println("START: N/A")
    }
    MyCCLib.set_debug(debug)
    MyCCLib.init_SymbTable()
    if doTime then {
      newtim = System.currentTimeMillis
      println(s"GRAAL_CONTEXT_STARTUP: ${newtim - old}ms")
      old = newtim
    }
    for (t <- MyCCLib.getAst) {
      try {
        if doTime then {
          newtim = System.currentTimeMillis
          println(s"LEXING_TIME: ${newtim - old}ms")
          old = newtim
        }
        val (cast, identPool) = MyCCLib.astToScala(t)
        if doTime then {
          newtim = System.currentTimeMillis
          println(s"IMPORTING_TO_SCALA: ${newtim - old}ms")
          old = newtim
        }
        val (context, ast) = parseCAst(cast, identPool)
        if doTime then {
          newtim = System.currentTimeMillis
          println(s"PARSE_CAST: ${newtim - old}ms")
          old = newtim
        }
        val (context2, astFlattened) = astToNormal(context, ast)
        if doTime then {
          newtim = System.currentTimeMillis
          println(s"AST_TO_NORMAL: ${newtim - old}ms")
          old = newtim
        }
        if doPrintNormal then {
          if doSeparate then {
            println("code:")
          }          
          printAst(context2, astFlattened)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"PRINTING_CODE: ${newtim - old}ms")
            old = newtim
          }
        }
        if doInterpret then {
          interpretAst(context2, astFlattened)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"INTERPRETING NORMAL: ${newtim - old}ms")
            old = newtim
          }
        }
        val (context3, tac) = normalToTac(context2, astFlattened)
        if doTime then {
          newtim = System.currentTimeMillis
          println(s"NORMAL_TO_TAC: ${newtim - old}ms")
          old = newtim
        }
        if doPrintTac then {
          if doSeparate then {
            println("TAC:")
          }
          printAst(context3, tac)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"PRINTING_TAC: ${newtim - old}ms")
            old = newtim
          }
        }
        val (context4, mips) = tacToMips(context3, tac)
        if doTime then {
          newtim = System.currentTimeMillis
          println(s"TAC_TO_MIPS: ${newtim - old}ms")
          old = newtim
        }
        if doPrintMips then {
          if doSeparate then {
            println("MIPS:")
          }
          printMips(context4, mips)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"PRINT_MIPS: ${newtim - old}ms")
            old = newtim
          }
        }
      } catch {
        case e: (SemanticError | UnexpectedAstNode | UnimplementedError) =>
          Console.err.println(s"[ERROR] $e")
      }
    }
  }
}