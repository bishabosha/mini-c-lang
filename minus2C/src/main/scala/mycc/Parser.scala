package mycc

import org.graalvm.polyglot._

object Parser {
  def main(aStrings: Array[String]): Unit = {
    val debug: java.lang.Boolean = aStrings.nonEmpty && "-d" == aStrings(0)
    MyCCLib.set_debug(debug)
    MyCCLib.init_symbtable()
    println("--C COMPILER")
    MyCCLib.yyparse()
    println(s"symbols: ${MyCCLib.get_SymbTable_inst}")
    val tree: Option[Value] = Option(MyCCLib.get_ans)
    val treePointer = tree.map(_.asNativePointer).getOrElse(0L)
    printf("parse finished with 0x%08X\n", treePointer)
    MyCCLib.print_ast(tree.filter(!_.isNull).orNull)
  }
}