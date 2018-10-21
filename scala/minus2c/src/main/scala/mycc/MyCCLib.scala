package mycc

import java.io._

import org.graalvm.polyglot._

object MyCCLib {
  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val source = Source.newBuilder("llvm", getClass.getResource("/mycclib")).build
  private val myCCLib = polyglot.eval(source)
  
  def init_SymbTable(): Unit = myCCLib.getMember("init_SymbTable").executeVoid()
  def yyparse(): Unit = myCCLib.getMember("yyparse").executeVoid()
  def set_debug(value: java.lang.Boolean): Unit = myCCLib.getMember("set_debug").executeVoid(value)
  def get_ans: Value = myCCLib.getMember("get_ans").execute()
  def print_ast(ast: Value): Unit = myCCLib.getMember("print_ast").executeVoid(ast)
  def get_SymbTable_inst: SymbTable = myCCLib.getMember("get_SymbTable_inst").execute().as(classOf[SymbTable])
  def accept_ast_visitor: Value = myCCLib.getMember("accept_ast_visitor")
  def accept_ast: Value = myCCLib.getMember("accept_ast")
  def sendValue(value: Any): Value = polyglot.asValue(value)

  def astToScala(ast: Value): CAst = {
    myCCLib.getMember("Ast_to_Scala").executeVoid(ast)
    myCCLib.getMember("get_deque")
           .execute()
           .as(classOf[java.util.ArrayDeque[CAst]])
           .pop
  }

  def close(): Unit = polyglot.close()
}