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
object mmclib
  import opaques._
  import AstTag._
  import CAst.given
  import AstInfo.{given, _}

  private val polyglot = Context.newBuilder().allowAllAccess(true).build
  private val source   = Source.newBuilder("llvm", getClass.getResource("/mmclib")).build
  private val mmclib   = polyglot.eval(source)

  private object opaques

    opaque type AstInfo = Value

    opaque type CAst                   = Value
    opaque type BinaryNodeOps  <: CAst = Value
    opaque type UnaryNodeOps   <: CAst = Value
    opaque type TokenStringOps <: CAst = Value
    opaque type TokenIntOps    <: CAst = Value
    opaque type SingletonOps   <: CAst = Value

    object BinaryNodeOps
      given :(node: BinaryNodeOps)
        def a1: CAst = UnaryNode_a1.execute(node)
        def a2: CAst = BinaryNode_a2.execute(node)

    object UnaryNodeOps
      given :(node: UnaryNodeOps)
        def a1: CAst = UnaryNode_a1.execute(node)

    object TokenIntOps
      given :(node: TokenIntOps)
        def value: Int = TokenInt_value.execute(node).asInt

    object TokenStringOps
      given :(node: TokenStringOps)
        def lexeme: String = TokenString_lexeme.execute(node).asString

    def parse(): Option[CAst] = Option(get_ast.execute()).filter(!_.isNull)

    object CAst
      given :(node: CAst)

        def ast: AstInfo =
          if node.hasMember("ast") then
            node.getMember("ast")
          else
            node

        def nonEmpty: Boolean = (node `ne` null) && !node.isNull

    object AstInfo

      private val AstTags = AstTag.values.sortWith(_.ordinal < _.ordinal)

      given :(node: AstInfo)
        def tpe: String = Ast_tpe.execute(node).asString
        def tag: AstTag = AstTags(Ast_tag.execute(node).asInt)


    val EmptyAst: CAst = null_Ast

    def free(node: CAst): Unit = free_pointer.executeVoid(node)

  export opaques._

  // Keep in sync with ast.h `AstTag`
  enum AstTag { case Singleton, UnaryNode, BinaryNode, TokenInt, TokenString }

  inline given singleton(given node: SingletonOps): SingletonOps = node
  inline given binary(given node: BinaryNodeOps): BinaryNodeOps = node
  inline given unary(given node: UnaryNodeOps): UnaryNodeOps = node
  inline given tokenString(given node: TokenStringOps): TokenStringOps = node
  inline given tokenInt(given node: TokenIntOps): TokenIntOps = node

  inline def (node: CAst) castTo[Ops, T](tag: AstTag)(cond: => CAst => Boolean)(f: => (given Ops) => T): Option[T] =
    node.ast.tag match
      case `tag` if cond(node) => Some(f(given node.asInstanceOf))
      case _                   => None

  inline def (node: CAst) castOp[Ops, T](tag: AstTag)(cond: => CAst => Boolean)(op: => (given Ops) => Unit): Unit =
    node.ast.tag match
      case `tag` if cond(node) => op(given node.asInstanceOf)
      case _                   =>

  inline def (node: CAst) asBinaryNode[T](f: => (given BinaryNodeOps) => T) =
    node.castTo(BinaryNode)(n => !sequenceTpes.contains(n.ast.tpe))(f)

  inline def (node: CAst) asSequence[T](f: => (given BinaryNodeOps) => T) =
    node.castTo(BinaryNode)(n => sequenceTpes.contains(n.ast.tpe))(f)

  inline def (node: CAst) asUnaryNode[T](f: => (given UnaryNodeOps) => T)     = node.castTo(UnaryNode)(True)(f)
  inline def (node: CAst) asTokenInt[T](f: => (given TokenIntOps) => T)       = node.castTo(TokenInt)(True)(f)
  inline def (node: CAst) asTokenString[T](f: => (given TokenStringOps) => T) = node.castTo(TokenString)(True)(f)
  inline def (node: CAst) asSingleton[T](f: => (given SingletonOps) => T)     = node.castTo(Singleton)(True)(f)
  inline def (node: CAst) binaryOp(op: => (given BinaryNodeOps) => Unit)      = node.castOp(BinaryNode)(True)(op)

  inline def (node: CAst) cata[T](
    unaop: => (given UnaryNodeOps) => T,
    binop: => (given BinaryNodeOps) => T,
    tokst: => (given TokenStringOps) => T,
    tokin: => (given TokenIntOps) => T,
    singl: => (given SingletonOps) => T
  ): T = node.ast.tag match
    case UnaryNode   => unaop(given node.asInstanceOf)
    case BinaryNode  => binop(given node.asInstanceOf)
    case TokenString => tokst(given node.asInstanceOf)
    case TokenInt    => tokin(given node.asInstanceOf)
    case Singleton   => singl(given node.asInstanceOf)

  private val sequenceTpes = Set("E", ";", ",", "~")

  def initSymbTable(): Unit =
    init_SymbTable.executeVoid()

  def setDebug(value: Boolean): Unit =
    set_debug.executeVoid(Boolean.box(value))

  def identPool: Set[Identifier] =
    val symbTable = get_SymbTable_inst.execute().asHostObject[SymbTable]
    val symbols   = symbTable.toSet
    symbTable.clear()
    symbols

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
