package mycc

import org.graalvm.polyglot._
import exception._
import CAst._

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
        // println(cast)
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
        // println(ast)
        // printScopesOrdered(context)
        val (nContext, astFlattened) = astToNormal(context, ast)
        if doTime then {
          newtim = System.currentTimeMillis
          println(s"AST_TO_NORMAL: ${newtim - old}ms")
          old = newtim
        }
        if doPrintNormal then {
          if doSeparate then {
            println("NORMAL:")
          }          
          printAst(astFlattened)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"PRINTING_CODE: ${newtim - old}ms")
            old = newtim
          }
        }
        if doInterpret then {
          val (intContext, intCode) =
            normalToInterpreter(nContext, astFlattened)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"FILTER_NORMAL_TO_INTERPRETER: ${newtim - old}ms")
            old = newtim
          }
          if doSeparate then {
            println("NORMAL_FILTERED:")
          }
          printAst(intCode)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"PRINTING_NORMAL_FOR_INTERPRETER: ${newtim - old}ms")
            old = newtim
          }
          interpretAst(intContext, intCode)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"INTERPRETING NORMAL: ${newtim - old}ms")
            old = newtim
          }
        } else {
          val (tContext2, tac2) = normalToTac(astFlattened)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"NORMAL_TO_TAC: ${newtim - old}ms")
            old = newtim
          }
          if doPrintTac then {
            if doSeparate then {
              println("TAC:")
            }
            printTac(tContext2,tac2)
            if doTime then {
              newtim = System.currentTimeMillis
              println(s"PRINTING_TAC: ${newtim - old}ms")
              old = newtim
            }
          }
          val mips2 = tacToMips(tContext2, tac2)
          if doTime then {
            newtim = System.currentTimeMillis
            println(s"TAC_TO_MIPS: ${newtim - old}ms")
            old = newtim
          }
          if doPrintMips then {
            if doSeparate then {
              println("MIPS:")
            }
            printMips(mips2)
            if doTime then {
              newtim = System.currentTimeMillis
              println(s"PRINT_MIPS: ${newtim - old}ms")
              old = newtim
            }
          }
        }
      } catch {
        case e: (SemanticError | UnexpectedAstNode | UnimplementedError) =>
          Console.err.println(s"[ERROR] $e")
      }
    }
  }
}