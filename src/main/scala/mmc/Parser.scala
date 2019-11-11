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

  if doTime then info("START: 0ms")

  initmmclib()

  mmclib.setDebug(debug)
  mmclib.initSymbTable()

  for nativeAst <- parseNativeAst() do try
    val identifiers = exportIdentifiers()
    printNativeAst(nativeAst)
    val (context, ast) = astFromNative(nativeAst, identifiers)
    val (nContext, normalAst) = aNormalForm(context, ast)
    printNormalAst(normalAst)
    if doInterpret then
      val interpreterAst = interpreterCode(normalAst)
      printInterpreterAst(interpreterAst)
      interpret(nContext, interpreterAst)
    else
      val (tacContext, tac) = tacCode(normalAst)
      printTacCode(tacContext, tac)
      printMipsCode(mipsCode(tacContext, tac))
  catch
    case e: (SemanticError | UnexpectedAstNode | UnimplementedError) =>
      Console.err.println(s"[ERROR] $e")

  def initmmclib() =
    timed("GRAAL_CONTEXT_STARTUP"):
      mmclib // force it to load
      ()

  def parseNativeAst() =
    timed("PARSE_TEXT"):
      mmclib.parse()

  def astFromNative(nativeAst: mmclib.CAst, identifiers: Set[Identifier]) =
    timed("NATIVE_AST_TO_SCALA"):
      parseCAst(nativeAst, identifiers) // TODO -- reentrant: - free nativeAst

  def exportIdentifiers() =
    timed("EXPORT_IDENTIFIERS"):
      guard(mmclib.identifiers): // TODO -- concurrent: - partition indent pool by job id
        mmclib.clearIdentPool()

  def aNormalForm(context: parseCAst.Context, ast: parseCAst.Goal) =
    timed("AST_TO_NORMAL"):
      astToNormal(context, ast)

  def interpreterCode(normalAst: astToNormal.Goal) =
    timed("FILTER_NORMAL_TO_INTERPRETER"):
      normalToInterpreter(normalAst)

  def mipsCode(tacContext: normalToTac.Context, tac: normalToTac.Goal) =
    timed("TAC_TO_MIPS"):
      tacToMips(tacContext, tac)

  def interpret(interpreterContext: normalToInterpreter.Context, interpreterAst: normalToInterpreter.Goal) =
    timed("INTERPRETING_NORMAL"):
      interpretAst(interpreterContext, interpreterAst)

  def tacCode(normalAst: astToNormal.Goal) =
    timed("NORMAL_TO_TAC"):
      normalToTac(normalAst)

  def printNativeAst(nativeAst: mmclib.CAst) =
    printStage(name = "PRINT_AST", label = "AST", guard = doPrintAst):
      println(DSL.show(nativeAst))

  def printNormalAst(normalAst: astToNormal.Goal) =
    printStage(name = "PRINTING_CODE", label = "NORMAL", guard = doPrintNormal):
      printAst(normalAst)

  def printInterpreterAst(interpreterAst: normalToInterpreter.Goal) =
    printStage(name = "PRINTING_NORMAL_FOR_INTERPRETER", label = "NORMAL_FILTERED", guard = doPrintInt):
        printAst(interpreterAst)

  def printTacCode(tacContext: normalToTac.Context, tac: normalToTac.Goal) =
    printStage(name = "PRINTING_TAC", label = "TAC", guard = doPrintTac):
      printTac(tacContext, tac)

  def printMipsCode(mips: tacToMips.Goal) =
    printStage(name = "PRINT_MIPS", label = "MIPS", guard = doPrintMips):
      printMips(mips)

  inline def timed[A](label: String)(op: => A): A = guard(op)(timeIt(label))

  inline def guard[A](op: => A)(guard: => Unit): A = { val a = op; guard; a }

  def printStage(name: String, label: String, guard: Boolean)(op: => Unit): Unit =
    if guard then
      if doSeparate then info(label + ":")
      op
      timeIt(name)

  def timeIt(label: String): Unit =
    if doTime then
      newtime = System.currentTimeMillis
      info(s"$label: ${newtime - old}ms")
      old = newtime

  def info(msg: String) = println("[INFO] " + msg)
