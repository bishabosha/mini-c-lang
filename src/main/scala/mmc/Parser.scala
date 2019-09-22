package mmc

import exception._

@main def Parser(args: String*) = {
  val debug         = args.contains("-debug")
  val doPrint       = args.contains("-p")
  val doTime        = args.contains("-time")
  val doInterpret   = args.contains("-i")
  val doPrintNormal = args.contains("-n")
  val doPrintMips   = args.contains("-m")
  val doPrintTac    = args.contains("-t")
  val doSeparate    = args.contains("-sep")
  var old           = System.currentTimeMillis
  var newtime       = 0L

  if doTime then println("START: 0ms")

  mmclib // force it to load

  timeIt("GRAAL_CONTEXT_STARTUP")

  mmclib.setDebug(debug)
  mmclib.initSymbTable()

  for t <- mmclib.parse() do try {
    timeIt("LEXING_TIME")
    val identPool = mmclib.identPool
    timeIt("EXPORT_IDENTIFIERS")
    val cast = t
    if doPrint then {
      println("AST:")
      DSL.show(t)
      timeIt("PRINT_AST")
    }
    val (context, ast) = parseCAst(cast, identPool) // TODO: Re-entrant - free memory
    timeIt("PARSE_CAST")
    val (nContext, astFlattened) = astToNormal(context, ast)
    timeIt("AST_TO_NORMAL")
    if doPrintNormal then {
      if doSeparate then println("NORMAL:")
      printAst(astFlattened)
      timeIt("PRINTING_CODE")
    }
    if doInterpret then {
      val (intContext, intCode) =
        normalToInterpreter(nContext, astFlattened)
      timeIt("FILTER_NORMAL_TO_INTERPRETER")
      if doSeparate then println("NORMAL_FILTERED:")
      printAst(intCode)
      timeIt("PRINTING_NORMAL_FOR_INTERPRETER")
      interpretAst(intContext, intCode)
      timeIt("INTERPRETING_NORMAL")
    }
    else {
      val (tContext2, tac2) = normalToTac(astFlattened)
      timeIt("NORMAL_TO_TAC")
      if doPrintTac then {
        if doSeparate then println("TAC:")
        printTac(tContext2,tac2)
        timeIt("PRINTING_TAC")
      }
      val mips2 = tacToMips(tContext2, tac2)
      timeIt("TAC_TO_MIPS")
      if doPrintMips then {
        if doSeparate then println("MIPS:")
        printMips(mips2)
        timeIt("PRINT_MIPS")
      }
    }
  } catch {
    case e: (SemanticError | UnexpectedAstNode | UnimplementedError) =>
      Console.err.println(s"[ERROR] $e")
  }

  def timeIt(label: String): Unit =
    if doTime then {
      newtime = System.currentTimeMillis
      println(s"$label: ${newtime - old}ms")
      old = newtime
    }
}
