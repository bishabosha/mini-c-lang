package mmc

import scala.annotation.targetName
import scala.annotation.switch
import scala.collection.mutable

import java.io._

import org.graalvm.polyglot._

/**Wrapper around llvm bitcode library of same name.
 *
 * Exports opaque type [[CAst]] which is simply a polyglot value with a type safe api, representing the AST built by
 * the Yacc parser.
 */
object mmclib:
  import opaques._
  import AstTag._
  import CAst.given
  import AstInfo.{given, _}

  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val source   = Source.newBuilder("llvm", getClass.getResource("/mmclib")).build
  private val mmclib   = polyglot.eval(source)

  private object opaques:

    opaque type AstInfo = Value

    opaque type CAst                   = Value
    opaque type BinaryNodeOps  <: CAst = Value
    opaque type UnaryNodeOps   <: CAst = Value
    opaque type TokenStringOps <: CAst = Value
    opaque type TokenIntOps    <: CAst = Value
    opaque type SingletonOps   <: CAst = Value

    extension (node: BinaryNodeOps)
      def a1: CAst = UnaryNode_a1.execute(node)
      def a2: CAst = BinaryNode_a2.execute(node)

    extension (node: UnaryNodeOps)
      @targetName("unaryA1")
      def a1: CAst = UnaryNode_a1.execute(node)

    extension (node: TokenIntOps)
      def value: Int = TokenInt_value.execute(node).asInt

    extension (node: TokenStringOps)
      def lexeme: String = TokenString_lexeme.execute(node).asString

    def parse(): Option[CAst] = Option(get_ast.execute()).filter(!_.isNull)

    object CAst:

      extension (node: CAst)

        def ast: AstInfo =
          if node.hasMember("ast") then
            node.getMember("ast")
          else
            node

        def nonEmpty: Boolean = (node `ne` null) && !node.isNull

    end CAst

    object AstInfo:

      private val AstTags = AstTag.values.sortWith(_.ordinal < _.ordinal)

      extension (node: AstInfo)
        def tpe: String = Ast_tpe.execute(node).asString
        def tag: AstTag = AstTags(Ast_tag.execute(node).asInt)

    end AstInfo

    val EmptyAst: CAst = null_Ast

    def free(node: CAst): Unit = free_pointer.executeVoid(node)

  export opaques._

  enum AstTag derives CanEqual:

    case // Keep in sync with ast.h `AstTag`
      Singleton,
      UnaryNode,
      BinaryNode,
      TokenInt,
      TokenString

    def isBinary: Boolean = this `eq` BinaryNode

  end AstTag

  inline given singleton(using node: SingletonOps): SingletonOps = node
  inline given binary(using node: BinaryNodeOps): BinaryNodeOps = node
  inline given unary(using node: UnaryNodeOps): UnaryNodeOps = node
  inline given tokenString(using node: TokenStringOps): TokenStringOps = node
  inline given tokenInt(using node: TokenIntOps): TokenIntOps = node

  extension [Ops, T](node: CAst)
    private inline def castTo(tag: AstTag)(cond: => CAst => Boolean)(f: => Ops ?=> T): Option[T] =
      node.ast.tag match
      case `tag` if cond(node) => Some(f(using node.asInstanceOf))
      case _                   => None
    end castTo

  extension [Ops, T](node: CAst)
    private inline def castOp(tag: AstTag)(cond: => CAst => Boolean)(op: => Ops ?=> Unit): Unit =
      node.ast.tag match
      case `tag` if cond(node) => op(using node.asInstanceOf)
      case _                   =>
    end castOp

  extension [T](node: CAst) inline def asBinaryNode(f: => BinaryNodeOps ?=> T) =
    node.castTo(BinaryNode)(n => !sequenceTpes.contains(n.ast.tpe))(f)

  extension [T](node: CAst) inline def asSequence(f: => BinaryNodeOps ?=> T) =
    node.castTo(BinaryNode)(n => sequenceTpes.contains(n.ast.tpe))(f)

  extension [T](node: CAst) inline def asUnaryNode(f: => UnaryNodeOps ?=> T)     = node.castTo(UnaryNode)(True)(f)
  extension [T](node: CAst) inline def asTokenInt(f: => TokenIntOps ?=> T)       = node.castTo(TokenInt)(True)(f)
  extension [T](node: CAst) inline def asTokenString(f: => TokenStringOps ?=> T) = node.castTo(TokenString)(True)(f)
  extension [T](node: CAst) inline def asSingleton(f: => SingletonOps ?=> T)     = node.castTo(Singleton)(True)(f)
  extension (node: CAst) inline def binaryOp(op: => BinaryNodeOps ?=> Unit)      = node.castOp(BinaryNode)(True)(op)

  extension [T](node: CAst) inline def cata(
    unaop: => UnaryNodeOps ?=> T,
    binop: => BinaryNodeOps ?=> T,
    tokst: => TokenStringOps ?=> T,
    tokin: => TokenIntOps ?=> T,
    singl: => SingletonOps ?=> T
  ): T = node.ast.tag match
    case UnaryNode   => unaop(using node.asInstanceOf)
    case BinaryNode  => binop(using node.asInstanceOf)
    case TokenString => tokst(using node.asInstanceOf)
    case TokenInt    => tokin(using node.asInstanceOf)
    case Singleton   => singl(using node.asInstanceOf)

  private val sequenceTpes = Set("E", ";", ",", "~")

  def initSymbTable(): Unit =
    init_SymbTable.executeVoid()

  def setDebug(value: Boolean): Unit =
    set_debug.executeVoid(Boolean.box(value))

  def clearIdentPool(): Unit = get_SymbTable_inst.execute().asHostObject[SymbTable].clear()

  def identifiers: Set[Identifier] =
    get_SymbTable_inst.execute().asHostObject[SymbTable].toSet

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
