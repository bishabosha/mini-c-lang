package mycc

import java.io._

import org.graalvm.polyglot._

object MyCCLib {
  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val file = new File("mycclib")
  private val source = Source.newBuilder("llvm", file).build
  private val mycclib = polyglot.eval(source)

  private val _init_symbtable = mycclib.getMember("init_symbtable")
  private val _yyparse = mycclib.getMember("yyparse")
  private val _set_debug = mycclib.getMember("set_debug")
  private val _get_ans = mycclib.getMember("get_ans")
  private val _print_tree = mycclib.getMember("print_tree")

  def main(aStrings: Array[String]): Unit = {
    val debug: java.lang.Boolean = aStrings.nonEmpty && "-d" == aStrings(0)
    _set_debug(debug)
    _init_symbtable()
    println("--C COMPILER")
    _yyparse()

    val tree: Option[Value] = Option(_get_ans.execute())
    val treePointer = tree.map(_.asNativePointer).getOrElse(0L)
    printf("parse finished with 0x%08X\n", treePointer)
    _print_tree(tree.orNull)
  }
}