package mmc

import java.io._

import org.graalvm.polyglot._

/**Wrapper around llvm bitcode library of same name.
 *
 * Exports opaque type [[CAst]] which is simply a polyglot value with a type safe api, representing the AST built by
 * the Yacc parser.
 */
object mmclib {
  import opaques._
  import AstTag._
  import given CAst._
  import given AstInfo._

  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val source   = Source.newBuilder("llvm", getClass.getResource("/mmclib")).build
  private val mmclib   = polyglot.eval(source)

  private object opaques {
    opaque type CAst = org.graalvm.polyglot.Value
    opaque type AstInfo = org.graalvm.polyglot.Value

    def getAst: Option[CAst] = Option(get_ast.execute()).filter(!_.isNull)

    def (ast: CAst) nonEmpty: Boolean = (ast ne null) && !ast.isNull

    def (ast: CAst) ast: AstInfo =
      if ast.hasMember("ast") then
        ast.getMember("ast")
      else
        ast

    def (ast: AstInfo) tpe: String = get_type.execute(ast).asString
    def (ast: AstInfo) tag: AstTag = AstTags(get_tag.execute(ast).asInt)

    def (ast: CAst) a1: CAst = ast_to_poly.execute(ast.getMember("a1"))
    def (ast: CAst) a2: CAst = ast_to_poly.execute(ast.getMember("a2"))
    def (ast: CAst) lexeme: String = get_lexeme.execute(ast).asString
    def (ast: CAst) value: Int = get_value.execute(ast).asInt

    def free(ast: CAst): Unit = free_pointer.executeVoid(ast)
  }

  export opaques.{CAst, AstInfo, getAst, free}

  object CAst {
    given {
      def (value: CAst) ast: AstInfo   = opaques.ast(value)
      def (value: CAst) nonEmpty: Boolean = opaques.nonEmpty(value)
    }
  }

  object AstInfo {
    given {
      def (ast: AstInfo) tpe: String = opaques.tpe(ast)
      def (ast: AstInfo) tag: AstTag = opaques.tag(ast)
    }
  }

  enum AstTag { case SINGLETON, UNARY_NODE, BINARY_NODE, TOKEN_INT, TOKEN_STRING }

  private val AstTags = AstTag.values.sortWith(_.ordinal < _.ordinal)

  object BinaryNode {
    def unapply(node: CAst): Option[(String, CAst, CAst)] = {
      val ast = node.ast
      val tpe = ast.tpe
      ast.tag match {
        case BINARY_NODE if !sequenceTpes.contains(tpe) =>
          Some((tpe, node.a1, node.a2))
        case _ => None
      }
    }
  }

  object BinaryNodeOpt {
    def unapply(node: CAst): (CAst, CAst) = (node.a1, node.a2)
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

  private def (ast: CAst) sequence(tpe: String): List[CAst] = {
    var node = ast
    var list = List.empty[CAst]
    var reverse = false
    var decided = false
    val BinaryNodeOpt(left0, right0) = node
    var left  = left0
    var right = right0
    var break = false
    while (!break) {
      val leftinfo = left.ast
      val rightinfo = right.ast
      if (leftinfo.tag == BINARY_NODE && leftinfo.tpe == tpe) {
        if (!decided) {
          decided = true;
        }
        list = right :: list
        node = left
        val BinaryNodeOpt(left0, right0) = node
        left  = left0
        right = right0
      } else if (rightinfo.tag == BINARY_NODE && rightinfo.tpe == tpe) {
        if (!decided) {
          decided = true
          reverse = true
        }
        list = left :: list
        node = right
        val BinaryNodeOpt(left0, right0) = node
        left = left0
        right = right0
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

  def identPool: Map[String, Identifier] = {
    val symbTable = get_SymbTable_inst.execute().asHostObject[SymbTable]
    val symbols = symbTable.toMap
    symbTable.clear()
    symbols
  }

  def close(): Unit = polyglot.close()

  def printAst(ast: CAst): Unit = printAst0(ast, 0)

  private def printAst0(ast: CAst, level: Int): Unit =
    if ast.nonEmpty then {
      printLevel(level)
      val astInfo = ast.ast
      astInfo.tag match {
        case UNARY_NODE =>
          printTree(ast, level)
        case BINARY_NODE =>
          printBinaryTree(ast, level)
        case TOKEN_STRING =>
          printTokenString(ast)
        case TOKEN_INT =>
          printTokenInt(ast)
        case SINGLETON =>
          printSingleton(ast)
      }
    }

  private def printTree(node: CAst, level: Int): Unit = {
    print(s"${node.ast.tpe}\n")
    printAst0(node.a1, level + 2)
  }

  private def printBinaryTree(node: CAst, level: Int): Unit = {
    print(s"${node.ast.tpe}\n")
    printAst0(node.a1, level + 2)
    printAst0(node.a2, level + 2)
  }

  private def printTokenString(token: CAst) = {
    val info = token.ast
    info.tpe match {
      case "string" =>
        print("" + '"' + token.lexeme + '"' + '\n')
      case _ =>
        print(s"${token.lexeme}\n")
    }
  }

  private def printTokenInt(token: CAst) = print(s"${token.value}\n")

  private def printSingleton(ast: CAst) = print(s"${ast.ast.tpe}\n")

  private def printLevel(level: Int): Unit = print(" " * level)

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