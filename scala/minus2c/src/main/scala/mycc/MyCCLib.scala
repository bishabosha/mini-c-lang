package mycc

import java.io._

import org.graalvm.polyglot._

object MyCCLib {
  private val polyglot =
    Context.newBuilder().allowAllAccess(true)
    // .option("llvm.sandboxed","true")
    .build

  private val source =
    Source.newBuilder("llvm", getClass.getResource("/mycclib")).build

  private val myCCLib =
    polyglot.eval(source)
  
  def init_SymbTable(): Unit =
    myCCLib.getMember("init_SymbTable").executeVoid()

  def set_debug(value: java.lang.Boolean): Unit =
    myCCLib.getMember("set_debug").executeVoid(value)

  def getAst: Option[Value] =
    Option(myCCLib.getMember("get_ast").execute()).filter(!_.isNull)

  def print_ast(ast: Value): Unit =
    myCCLib.getMember("print_ast").executeVoid(ast)

  def astToScala(ast: Value): (CAst, Map[String, Identifier]) = {
    (myCCLib.getMember("Ast_to_Scala").execute(ast)
           .as(classOf[CAst]), {
             val symbTable =
              myCCLib.getMember("get_SymbTable_inst")
                     .execute()
                     .as(classOf[SymbTable])
             val symbols = symbTable.toMap
             symbTable.clear()
             symbols
           })
  }

  def close(): Unit = polyglot.close()
}