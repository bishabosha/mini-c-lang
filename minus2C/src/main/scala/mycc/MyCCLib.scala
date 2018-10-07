package mycc

import java.io._

import org.graalvm.polyglot._

object MyCCLib {
  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val file = new File("mycclib")
  private val source = Source.newBuilder("llvm", file).build
  private val mycclib = polyglot.eval(source)

  private val init_symbtable = mycclib.getMember("init_symbtable")
  private val yyparse = mycclib.getMember("yyparse")
  private val set_debug = mycclib.getMember("set_debug")
  private val get_ans = mycclib.getMember("get_ans")
  private val print_tree = mycclib.getMember("print_tree")

  def main(aStrings: Array[String]): Unit = {
    val debug: java.lang.Boolean = aStrings.nonEmpty && "-d" == aStrings(0)
    set_debug.executeVoid(debug)
    init_symbtable.executeVoid()
    println("--C COMPILER")
    yyparse.executeVoid()

    val tree: Option[Value] = Option(get_ans.execute())
    val treePointer = tree.map(_.asNativePointer).getOrElse(0L)
    printf("parse finished with 0x%08X\n", treePointer)
    println(s"Symbols: $SymbTable")
    print_tree.executeVoid(tree.orNull)
  }
}