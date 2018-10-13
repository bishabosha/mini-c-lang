package mycc

import Ast._
import ArgList._

trait AstBase

enum Types {
  case int, void, function
}

enum Operarators {
  case +, -, /, *
}

enum StorageTypes {
  case auto, extern
}

enum ArgList {
  case LVoid
  case ListOf(list: Vector[(Type, Identifier)])
}

enum Ast {
  case ø
  case Identifier(id: String)
  case Constant(value: Int)
  case StringLiteral(value: String)
  case Type(id: Types)
  case Storage(id: StorageTypes)
  case If(cond: Ast, ifTrue: Ast, orElse: Option[Ast])
  case Return(value: Option[Ast])
  case Assignment(lvalue: Identifier, rvalue: Ast) // TODO: there is no multi-assignment in c
  case Declaration(storage: Storage, declType: Type, name: List[Identifier])
  case Function(storage: Storage, returnType: Type, name: Identifier, args: ArgList, body: Block)
  case Block(inner: List[Ast])
  case Leaf(kind: String, data: Either[String, Int])
  case Node0(kind: String)
  case Node1(kind: String, left: Ast)
  case Node2(kind: String, left: Ast, right: Ast)
}

object Ast {
  def storageOf(kind: StorageTypes): Storage = Storage(kind).asInstanceOf[Storage]
  def typeOf(kind: Types): Type = Type(kind).asInstanceOf[Type]
}