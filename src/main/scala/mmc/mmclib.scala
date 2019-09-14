package mmc

import java.io._

import org.graalvm.polyglot._

/**Wrapper around llvm bitcode library of same name.
 *
 * Exports opaque type [[CAst]] which is simply a polyglot value with a type safe api, representing the AST built by
 * the Yacc parser.
 */
object mmclib { self =>
  import opaques._
  import AstTag._
  import given CAst._
  import given AstInfo._

  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val source   = Source.newBuilder("llvm", getClass.getResource("/mmclib")).build
  private val mmclib   = polyglot.eval(source)

  private object opaques {
    opaque type CAst    = Value
    opaque type AstInfo = Value

    def getAst: Option[CAst] = Option(get_ast.execute()).filter(!_.isNull)

    object CAst {
      given (node: CAst) {
        def ast: AstInfo =
          if node.hasMember("ast") then
            node.getMember("ast")
          else
            node

        def nonEmpty: Boolean = (node ne null) && !node.isNull
        def printAst: String  = self.printAst(node)
      }
    }

    object AstInfo {
      given (node: AstInfo) {
        def tpe: String = get_type.execute(node).asString
        def tag: AstTag = AstTags(get_tag.execute(node).asInt)
      }
    }

    def (node: CAst) a1     : CAst   = ast_to_poly.execute(node.getMember("a1"))
    def (node: CAst) a2     : CAst   = ast_to_poly.execute(node.getMember("a2"))
    def (node: CAst) lexeme : String = get_lexeme.execute(node).asString
    def (node: CAst) value  : Int    = get_value.execute(node).asInt

    def free(node: CAst): Unit = free_pointer.executeVoid(node)
  }

  export opaques.{CAst, AstInfo, getAst, free}

  enum AstTag { case SINGLETON, UNARY_NODE, BINARY_NODE, TOKEN_INT, TOKEN_STRING }

  private val AstTags = AstTag.values.sortWith(_.ordinal < _.ordinal)

  object BinaryNode {
    def unapply(node: CAst): Option[(String, CAst, CAst)] = {
      val ast = node.ast
      val tpe = ast.tpe
      ast.tag match {
        case BINARY_NODE if !sequenceTpes.contains(tpe) => Some((tpe, node.a1, node.a2))
        case _                                          => None
      }
    }
  }

  object UnaryNode {
    def unapply(node: CAst): Option[(String, CAst)] = {
      val ast = node.ast
      ast.tag match {
        case UNARY_NODE => Some((ast.tpe, node.a1))
        case _          => None
      }
    }
  }

  object Singleton {
    def unapply(node: CAst): Option[String] = {
      val ast = node.ast
      ast.tag match {
        case SINGLETON => Some(ast.tpe)
        case _         => None
      }
    }
  }

  object TokenInt {
    def unapply(node: CAst): Option[(String, Int)] = {
      val ast = node.ast
      ast.tag match {
        case TOKEN_INT => Some((ast.tpe, node.value))
        case _         => None
      }
    }
  }

  object TokenString {
    def unapply(node: CAst): Option[(String, String)] = {
      val ast = node.ast
      ast.tag match {
        case TOKEN_STRING => Some((ast.tpe, node.lexeme))
        case _            => None
      }
    }
  }

  object Sequence {
    def unapply(node: CAst): Option[(String, List[CAst])] = {
      val ast = node.ast
      val tpe = ast.tpe
      ast.tag match {
        case BINARY_NODE if sequenceTpes.contains(tpe) => Some((tpe, node.sequence(tpe)))
        case _                                         => None
      }
    }
  }

  private val sequenceTpes = Set("E", ";", ",", "~")

  private def (node: CAst) sequence(tpe: String): List[CAst] = {
    var current = node
    var list = List.empty[CAst]
    var reverse = false
    var decided = false
    var left  = current.a1
    var right = current.a2
    var break = false
    while (!break) {
      val leftinfo = left.ast
      val rightinfo = right.ast
      if (leftinfo.tag == BINARY_NODE && leftinfo.tpe == tpe) {
        if (!decided) {
          decided = true;
        }
        list  = right :: list
        current  = left
        left  = current.a1
        right = current.a2
      } else if (rightinfo.tag == BINARY_NODE && rightinfo.tpe == tpe) {
        if (!decided) {
          decided = true
          reverse = true
        }
        list  = left :: list
        current  = right
        left  = current.a1
        right = current.a2
      } else {
        if (reverse) {
          list = right :: left :: list
        } else {
          list = left :: right :: list
        }
        break = true
      }
    }
    if reverse then list.reverse else list
  }

  def initSymbTable(): Unit =
    init_SymbTable.executeVoid()

  def setDebug(value: Boolean): Unit =
    set_debug.executeVoid(Boolean.box(value))

  def identPool: Set[Identifier] = {
    val symbTable = get_SymbTable_inst.execute().asHostObject[SymbTable]
    val symbols   = symbTable.toSet
    symbTable.clear()
    symbols
  }

  def close(): Unit = polyglot.close()

  def printAst(node: CAst): String = printAst0(node, 0, StringBuilder()).toString

  private def printAst0(node: CAst, level: Int, builder: StringBuilder): StringBuilder =
    if node.nonEmpty then {
      printLevel(level, builder)
      val ast = node.ast
      ast.tag match {
        case UNARY_NODE =>
          printUnaryNode(node, level, builder)
        case BINARY_NODE =>
          printBinaryNode(node, level, builder)
        case TOKEN_STRING =>
          printTokenString(node, builder)
        case TOKEN_INT =>
          printTokenInt(node, builder)
        case SINGLETON =>
          printSingleton(ast, builder)
      }
    } else {
      builder
    }

  private def printUnaryNode(node: CAst, level: Int, builder: StringBuilder): StringBuilder = {
    builder.addAll(s"${node.ast.tpe}\n")
    printAst0(node.a1, level + 2, builder)
  }

  private def printBinaryNode(node: CAst, level: Int, builder: StringBuilder): StringBuilder = {
    builder.addAll(s"${node.ast.tpe}\n")
    printAst0(node.a1, level + 2, builder)
    printAst0(node.a2, level + 2, builder)
  }

  private def printTokenString(node: CAst, builder: StringBuilder): StringBuilder = {
    val info = node.ast
    info.tpe match {
      case "string" =>
        builder.addAll("" + '"' + node.lexeme + '"' + '\n')
      case _ =>
        builder.addAll(s"${node.lexeme}\n")
    }
  }

  private def printTokenInt(node: CAst, builder: StringBuilder): StringBuilder = builder.addAll(s"${node.value}\n")

  private def printSingleton(ast: AstInfo, builder: StringBuilder): StringBuilder = builder.addAll(s"${ast.tpe}\n")

  private def printLevel(level: Int, builder: StringBuilder): StringBuilder = builder.addAll(" " * level)

  private val get_tag = mmclib.getMember("get_tag")
  private val get_type = mmclib.getMember("get_type")
  private val get_value = mmclib.getMember("get_value")
  private val get_lexeme = mmclib.getMember("get_lexeme")
  private val ast_to_poly = mmclib.getMember("ast_to_poly")
  private val free_pointer = mmclib.getMember("free_pointer")
  private val set_debug = mmclib.getMember("set_debug")
  private val get_ast = mmclib.getMember("get_ast")
  private val get_SymbTable_inst = mmclib.getMember("get_SymbTable_inst")
  private val init_SymbTable = mmclib.getMember("init_SymbTable")
}
