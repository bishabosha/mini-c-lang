package mycc

import java.io._

import org.graalvm.polyglot._

object MyCCLib {
  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val file = new File("mycclib")
  private val source = Source.newBuilder("llvm", file).build
  private val myCCLib = polyglot.eval(source)

  def init_symbtable(): Unit = myCCLib.getMember("init_symbtable").executeVoid()
  def yyparse(): Unit = myCCLib.getMember("yyparse").executeVoid()
  def set_debug(value: java.lang.Boolean): Unit = myCCLib.getMember("set_debug").executeVoid(value)
  def get_ans: Value = myCCLib.getMember("get_ans").execute()
  def print_ast(ast: Value): Unit = myCCLib.getMember("print_ast").executeVoid(ast)
  def get_SymbTable_inst: SymbTable = myCCLib.getMember("get_SymbTable_inst").execute().as(classOf[SymbTable])

  def close(): Unit = polyglot.close()
}