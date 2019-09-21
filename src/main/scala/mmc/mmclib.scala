package mmc

import scala.annotation.switch
import scala.collection.mutable

import java.io._

import org.graalvm.polyglot._

/**Wrapper around llvm bitcode library of same name.
 *
 * Exports opaque type [[CAst]] which is simply a polyglot value with a type safe api, representing the AST built by
 * the Yacc parser.
 */
object mmclib { self =>
  import opaques._
  import CAst.given
  import AstInfo.{given, _}

  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val source   = Source.newBuilder("llvm", getClass.getResource("/mmclib")).build
  private val mmclib   = polyglot.eval(source)

  private object opaques {

    opaque type CAst    = Value
    opaque type AstInfo = Value

    def parse(): Option[CAst] = Option(get_ast()).filter(!_.isNull)

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

      type AstTag = Singleton | UnaryNode | BinaryNode | TokenInt | TokenString

      private val AstTags = mutable.ArrayBuffer.empty[AstTag]
      private def enter[T <: AstTag](tag: T): Unit = { AstTags += tag }

      // Keep in sync with ast.h `AstTag`
      final class Singleton   private[AstInfo](); enter(Singleton())
      final class UnaryNode   private[AstInfo](); enter(UnaryNode())
      final class BinaryNode  private[AstInfo](); enter(BinaryNode())
      final class TokenInt    private[AstInfo](); enter(TokenInt())
      final class TokenString private[AstInfo](); enter(TokenString())

      AstTags.trimToSize()

      given (node: AstInfo) {
        def tpe: String = Ast_tpe(node).asString
        def tag: AstTag = AstTags(Ast_tag(node).asInt)
      }

    }

    def (node: CAst) a1 (given UnaryNode | BinaryNode): CAst = UnaryNode_a1(node)
    def (node: CAst) a2 (given BinaryNode): CAst = BinaryNode_a2(node)
    def (node: CAst) lexeme (given TokenString): String = TokenString_lexeme(node).asString
    def (node: CAst) value (given TokenInt): Int = TokenInt_value(node).asInt

    val EmptyAst: CAst = null_Ast

    def free(node: CAst): Unit = free_pointer.executeVoid(node)
  }

  export opaques.{CAst, AstInfo, EmptyAst, parse, free}

  object BinaryNode {
    def unapply(node: CAst): Option[(String, CAst, CAst)] = {
      val ast = node.ast
      val tpe = ast.tpe
      ast.tag match {
        case given _: BinaryNode if !sequenceTpes.contains(tpe) => Some((tpe, node.a1, node.a2))
        case _                                                  => None
      }
    }
  }

  object UnaryNode {
    def unapply(node: CAst): Option[(String, CAst)] = {
      val ast = node.ast
      ast.tag match {
        case given _: UnaryNode => Some((ast.tpe, node.a1))
        case _                  => None
      }
    }
  }

  object Singleton {
    def unapply(node: CAst): Option[String] = {
      val ast = node.ast
      ast.tag match {
        case _: Singleton => Some(ast.tpe)
        case _            => None
      }
    }
  }

  object TokenInt {
    def unapply(node: CAst): Option[(String, Int)] = {
      val ast = node.ast
      ast.tag match {
        case given _: TokenInt => Some((ast.tpe, node.value))
        case _                 => None
      }
    }
  }

  object TokenString {
    def unapply(node: CAst): Option[(String, String)] = {
      val ast = node.ast
      ast.tag match {
        case given _: TokenString => Some((ast.tpe, node.lexeme))
        case _                    => None
      }
    }
  }

  object Sequence {
    def unapply(node: CAst): Option[(String, List[CAst])] = {
      val ast = node.ast
      val tpe = ast.tpe
      ast.tag match {
        case given _: BinaryNode if sequenceTpes.contains(tpe) => Some((tpe, sequence(tpe, node.a1, node.a2)))
        case _                                                 => None
      }
    }
  }

  private val sequenceTpes = Set("E", ";", ",", "~")

  private def sequence(tpe: String, a1: CAst, a2: CAst): List[CAst] = {
    var left    = a1
    var right   = a2
    var list    = List.empty[CAst]
    var reverse = false
    var decided = false
    var break   = false
    while (!break) {
      val leftinfo = left.ast
      val rightinfo = right.ast
      if (leftinfo.tag.isInstanceOf[BinaryNode] && leftinfo.tpe == tpe) {
        if (!decided) {
          decided = true;
        }
        list = right :: list
        (left.ast.tag: @unchecked) match {
          case given _: BinaryNode =>
            right = left.a2
            left  = left.a1
        }
      } else if (rightinfo.tag.isInstanceOf[BinaryNode] && rightinfo.tpe == tpe) {
        if (!decided) {
          decided = true
          reverse = true
        }
        list  = left :: list
        (right.ast.tag: @unchecked) match {
          case given _: BinaryNode =>
            left  = right.a1
            right = right.a2
        }
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
    val symbTable = get_SymbTable_inst().asHostObject[SymbTable]
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
        case given _: UnaryNode   => node.printUnaryNode(level, builder)
        case given _: BinaryNode  => node.printBinaryNode(level, builder)
        case given _: TokenString => node.printTokenString(builder)
        case given _: TokenInt    => node.printTokenInt(builder)
        case given _: Singleton   => ast.printSingleton(builder)
      }
    } else {
      builder
    }

  private def (node: CAst) printUnaryNode(level: Int, builder: StringBuilder)(given UnaryNode): StringBuilder = {
    builder.addAll(s"${node.ast.tpe}\n")
    printAst0(node.a1, level + 2, builder)
  }

  private def (node: CAst) printBinaryNode(level: Int, builder: StringBuilder)(given BinaryNode): StringBuilder = {
    builder.addAll(s"${node.ast.tpe}\n")
    printAst0(node.a1, level + 2, builder)
    printAst0(node.a2, level + 2, builder)
  }

  private def (node: CAst) printTokenString(builder: StringBuilder)(given TokenString): StringBuilder = {
    val info = node.ast
    info.tpe match {
      case "string" =>
        builder.addAll("" + '"' + node.lexeme + '"' + '\n')
      case _ =>
        builder.addAll(s"${node.lexeme}\n")
    }
  }

  private def (node: CAst) printTokenInt(builder: StringBuilder)(given TokenInt): StringBuilder = builder.addAll(s"${node.value}\n")

  private def (ast: AstInfo) printSingleton(builder: StringBuilder)(given Singleton): StringBuilder = builder.addAll(s"${ast.tpe}\n")

  private def printLevel(level: Int, builder: StringBuilder): StringBuilder = builder.addAll(" " * level)

  private inline def (f: Value) apply(args: AnyRef*): Value = f.execute(args: _*)

  private val null_Ast = mmclib.getMember("null_Ast")
  private val Ast_tag = mmclib.getMember("Ast_tag")
  private val Ast_tpe = mmclib.getMember("Ast_tpe")
  private val UnaryNode_a1 = mmclib.getMember("UnaryNode_a1")
  private val BinaryNode_a2 = mmclib.getMember("BinaryNode_a2")
  private val TokenInt_value = mmclib.getMember("TokenInt_value")
  private val TokenString_lexeme = mmclib.getMember("TokenString_lexeme")
  private val free_pointer = mmclib.getMember("free_pointer")
  private val set_debug = mmclib.getMember("set_debug")
  private val get_ast = mmclib.getMember("get_ast")
  private val get_SymbTable_inst = mmclib.getMember("get_SymbTable_inst")
  private val init_SymbTable = mmclib.getMember("init_SymbTable")
}
