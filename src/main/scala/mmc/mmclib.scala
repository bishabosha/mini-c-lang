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

    opaque type CAst                   = Value
    opaque type BinaryNodeOps  <: CAst = Value
    opaque type UnaryNodeOps   <: CAst = Value
    opaque type TokenStringOps <: CAst = Value
    opaque type TokenIntOps    <: CAst = Value
    opaque type SingletonOps   <: CAst = Value

    opaque type AstInfo = Value

    object BinaryNodeOps {
      given (node: BinaryNodeOps) {
        def a1: CAst = UnaryNode_a1.execute(node)
        def a2: CAst = BinaryNode_a2.execute(node)
      }
    }

    object UnaryNodeOps {
      given (node: UnaryNodeOps) {
        def a1: CAst = UnaryNode_a1.execute(node)
      }
    }

    object TokenIntOps {
      given (node: TokenIntOps) {
        def value: Int = TokenInt_value.execute(node).asInt
      }
    }

    object TokenStringOps {
      given (node: TokenStringOps) {
        def lexeme: String = TokenString_lexeme.execute(node).asString
      }
    }

    def parse(): Option[CAst] = Option(get_ast.execute()).filter(!_.isNull)

    object CAst {
      import AstInfo.given

      given (node: CAst) {

        def ast: AstInfo =
          if node.hasMember("ast") then
            node.getMember("ast")
          else
            node

        def nonEmpty: Boolean = (node `ne` null) && !node.isNull
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
        def tpe: String = Ast_tpe.execute(node).asString
        def tag: AstTag = AstTags(Ast_tag.execute(node).asInt)
      }

    }

    val EmptyAst: CAst = null_Ast

    def free(node: CAst): Unit = free_pointer.executeVoid(node)
  }

  export opaques._

  inline given singleton(given node: SingletonOps): SingletonOps = node
  inline given binary(given node: BinaryNodeOps): BinaryNodeOps = node
  inline given unary(given node: UnaryNodeOps): UnaryNodeOps = node
  inline given tokenString(given node: TokenStringOps): TokenStringOps = node
  inline given tokenInt(given node: TokenIntOps): TokenIntOps = node

  inline def (node: CAst) asBinaryNode[T](f: => (given BinaryNodeOps) => T): Option[T] = node.ast.tag match {
    case _: BinaryNode if !sequenceTpes.contains(node.ast.tpe) => Some(f(given node.asInstanceOf))
    case _                                                     => None
  }

  inline def (node: CAst) asSequence[T](f: => (given BinaryNodeOps) => T): Option[T] = node.ast.tag match {
    case _: BinaryNode if sequenceTpes.contains(node.ast.tpe) => Some(f(given node.asInstanceOf))
    case _                                                    => None
  }

  inline def (node: CAst) asUnaryNode[T](f: => (given UnaryNodeOps) => T): Option[T] = node.ast.tag match {
    case _: UnaryNode => Some(f(given node.asInstanceOf))
    case _            => None
  }

  inline def (node: CAst) asTokenInt[T](f: => (given TokenIntOps) => T): Option[T] = node.ast.tag match {
    case _: TokenInt => Some(f(given node.asInstanceOf))
    case _           => None
  }

  inline def (node: CAst) asTokenString[T](f: => (given TokenStringOps) => T): Option[T] = node.ast.tag match {
    case _: TokenString => Some(f(given node.asInstanceOf))
    case _              => None
  }

  inline def (node: CAst) asSingleton[T](f: => (given SingletonOps) => T): Option[T] = node.ast.tag match {
    case _: Singleton => Some(f(given node.asInstanceOf))
    case _            => None
  }

  inline def (node: CAst) binaryOp[T](op: => (given BinaryNodeOps) => Unit): Unit = node.ast.tag match {
    case _: BinaryNode => op(given node.asInstanceOf)
    case _             =>
  }

  inline def (node: CAst) cata[T](
    unaop: => (given UnaryNodeOps) => T,
    binop: => (given BinaryNodeOps) => T,
    tokst: => (given TokenStringOps) => T,
    tokin: => (given TokenIntOps) => T,
    singl: => (given SingletonOps) => T
  ): T = node.ast.tag match {
    case _: UnaryNode   => unaop(given node.asInstanceOf)
    case _: BinaryNode  => binop(given node.asInstanceOf)
    case _: TokenString => tokst(given node.asInstanceOf)
    case _: TokenInt    => tokin(given node.asInstanceOf)
    case _: Singleton   => singl(given node.asInstanceOf)
  }

  private val sequenceTpes = Set("E", ";", ",", "~")

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

  private val null_Ast            = mmclib.getMember("null_Ast")
  private val Ast_tag             = mmclib.getMember("Ast_tag")
  private val Ast_tpe             = mmclib.getMember("Ast_tpe")
  private val UnaryNode_a1        = mmclib.getMember("UnaryNode_a1")
  private val BinaryNode_a2       = mmclib.getMember("BinaryNode_a2")
  private val TokenInt_value      = mmclib.getMember("TokenInt_value")
  private val TokenString_lexeme  = mmclib.getMember("TokenString_lexeme")
  private val free_pointer        = mmclib.getMember("free_pointer")
  private val set_debug           = mmclib.getMember("set_debug")
  private val get_ast             = mmclib.getMember("get_ast")
  private val get_SymbTable_inst  = mmclib.getMember("get_SymbTable_inst")
  private val init_SymbTable      = mmclib.getMember("init_SymbTable")

}
