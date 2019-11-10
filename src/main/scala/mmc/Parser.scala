package mmc

import exception._

@main def Parser(args: String*) =
  val debug         = args.contains("-debug")
  val doPrintAst    = args.contains("-p")
  val doTime        = args.contains("-time")
  val doInterpret   = args.contains("-i")
  val doPrintNormal = args.contains("-n")
  val doPrintInt    = args.contains("-int")
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

  for nativeAst <- mmclib.parse() do try
    timeIt("LEXING_TIME")
    val identifiers = timed("EXPORT_IDENTIFIERS"):
      guard(mmclib.identifiers): // TODO -- concurrent: - partition indent pool by job id
        mmclib.clearIdentPool()
    printStage(name = "PRINT_AST", label = "AST", guard = doPrintAst):
      println(DSL.show(nativeAst))
    val (context, ast) = timed("PARSE_CAST"):
      parseCAst(nativeAst, identifiers) // TODO -- reentrant: - free nativeAst
    val (nContext, astFlattened) = timed("AST_TO_NORMAL"):
      astToNormal(context, ast)
    printStage(name = "PRINTING_CODE", label = "NORMAL", guard = doPrintNormal):
      printAst(astFlattened)
    if doInterpret then
      val (intContext, intCode) = timed("FILTER_NORMAL_TO_INTERPRETER"):
        normalToInterpreter(nContext, astFlattened)
      printStage(name = "PRINTING_NORMAL_FOR_INTERPRETER", label = "NORMAL_FILTERED", guard = doPrintInt):
        printAst(intCode)
      timed("INTERPRETING_NORMAL"):
        interpretAst(intContext, intCode)
    else
      val (tacContext, tac) = timed("NORMAL_TO_TAC"):
        normalToTac(astFlattened)
      printStage(name = "PRINTING_TAC", label = "TAC", guard = doPrintTac):
        printTac(tacContext,tac)
      val mips = timed("TAC_TO_MIPS"):
        tacToMips(tacContext, tac)
      printStage(name = "PRINT_MIPS", label = "MIPS", guard = doPrintMips):
        printMips(mips)
  catch
    case e: (SemanticError | UnexpectedAstNode | UnimplementedError) =>
      Console.err.println(s"[ERROR] $e")

  inline def timed[A](label: String)(op: => A): A = guard(op)(timeIt(label))

  inline def guard[A](op: => A)(guard: => Unit): A = { val a = op; guard; a }

  def printStage(name: String, label: String, guard: Boolean)(op: => Unit): Unit =
    if guard then
      if doSeparate then println(label + ":")
      op
      timeIt(name)

  def timeIt(label: String): Unit =
    if doTime then
      newtime = System.currentTimeMillis
      println(s"$label: ${newtime - old}ms")
      old = newtime
