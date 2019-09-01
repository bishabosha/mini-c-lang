package mmc

import java.io._

import org.graalvm.polyglot._

object mmclib {
  import AstTag._
  import given Ast._

  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val source   = Source.newBuilder("llvm", getClass.getResource("/mmclib")).build
  private val mmclib   = polyglot.eval(source)

  opaque type CAst = org.graalvm.polyglot.Value
  opaque type AstInfo = org.graalvm.polyglot.Value

  case class AstKind(tag: AstTag, tpe: String)

  object Ast {
    given {

      def (value: CAst) ast: AstInfo =
        if value.hasMember("ast") then
          value.getMember("ast")
        else
          value

      def (value: CAst) kind: AstKind = {
        val ast = value.ast
        AstKind(ast.tag, ast.tpe)
      }

    }

    given {
      def (ast: AstInfo) tpe: String = get_type.execute(ast).asString
      def (ast: AstInfo) tag: AstTag = {
        val ordinal = get_tag.execute(ast).asInt
        AstTag.values.find(_.ordinal == ordinal).get
      }
    }
  }

  enum AstTag { case SINGLETON, UNARY_NODE, BINARY_NODE, TOKEN_INT, TOKEN_STRING }

  object BinaryNode {
    def unapply(ast: CAst): Option[(String, CAst, CAst)] = ast.kind match {
      case AstKind(BINARY_NODE, tpe) if !sequenceTpes.contains(tpe) =>
        Some((tpe, poly(ast.getMember("a1")), poly(ast.getMember("a2"))))
      case _ => None
    }
  }

  object BinaryNodeOpt {
    def unapply(ast: CAst): (CAst, CAst) = (poly(ast.getMember("a1")), poly(ast.getMember("a2")))
  }

  object UnaryNode {
    def unapply(ast: CAst): Option[(String, CAst)] = ast.kind match {
      case AstKind(UNARY_NODE, tpe) => Some((tpe, poly(ast.getMember("a1"))))
      case _ => None
    }
  }

  object Singleton {
    def unapply(ast: CAst): Option[String] = ast.kind match {
      case AstKind(SINGLETON, tpe) => Some(tpe)
      case _ => None
    }
  }

  object TokenInt {
    def unapply(ast: CAst): Option[(String, Int)] = ast.kind match {
      case AstKind(TOKEN_INT, tpe) => Some((tpe, value(ast)))
      case _ => None
    }
  }

  object TokenString {
    def unapply(ast: CAst): Option[(String, String)] = ast.kind match {
      case AstKind(TOKEN_STRING, tpe) => Some((tpe, lexeme(ast)))
      case _ => None
    }
  }

  object Sequence {
    def unapply(ast: CAst): Option[(String, List[CAst])] = ast.kind match {
      case AstKind(BINARY_NODE, tpe) if sequenceTpes.contains(tpe) => Some((tpe, sequence(tpe, ast)))
      case _ => None
    }
  }

  val sequenceTpes = Set("E", ";", ",", "~")

  private def sequence(tpe: String, ast: CAst): List[CAst] = {
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

  private def free(ast: CAst): Unit = {
    free_pointer.executeVoid(ast)
  }

  def initSymbTable(): Unit =
    init_SymbTable.executeVoid()

  def setDebug(value: Boolean): Unit =
    set_debug.executeVoid(Boolean.box(value))

  def getAst: Option[CAst] =
    Option(get_ast.execute()).filter(!_.isNull)

  def identPool: Map[String, Identifier] = {
    val symbTable = get_SymbTable_inst.execute().asHostObject[SymbTable]
    val symbols = symbTable.toMap
    symbTable.clear()
    symbols
  }

  def close(): Unit = polyglot.close()

  def printAst(ast: CAst): Unit = printAst0(ast, 0)

  private def printAst0(ast: CAst, level: Int): Unit =
    if (ast ne null) && !ast.isNull then {
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
          printSingleton(astInfo)
      }
    }

  private def printTree(node: CAst, level: Int): Unit = {
    print(s"${node.ast.tpe}\n")
    printAst0(poly(node.getMember("a1")), level + 2)
  }

  private def printBinaryTree(node: CAst, level: Int): Unit = {
    print(s"${node.ast.tpe}\n")
    printAst0(poly(node.getMember("a1")), level + 2)
    printAst0(poly(node.getMember("a2")), level + 2)
  }

  private def printTokenString(token: CAst) = {
    val info = token.ast
    info.tpe match {
      case "string" =>
        print("" + '"' + lexeme(token) + '"' + '\n')
      case _ =>
        print(s"${lexeme(token)}\n")
    }
  }

  private def printTokenInt(token: CAst) = print(s"${value(token)}\n")

  private def printSingleton(ast: CAst) = print(s"${ast.ast.tpe}\n")

  private def printLevel(level: Int): Unit = print(" " * level)

  private def poly(ast: CAst): CAst = ast_to_poly.execute(ast)
  private def lexeme(ast: CAst): String = get_lexeme.execute(ast).asString
  private def value(ast: CAst): Int = get_value.execute(ast).asInt

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