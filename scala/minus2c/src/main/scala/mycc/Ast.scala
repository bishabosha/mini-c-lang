package mycc

import Ast._

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
  case LVoid, LAny
  case LParam(list: Vector[(Types, Identifier)])
}

case object LVoid

enum Ast {
  case ø
  case Identifier(id: String)
  case Constant(value: Int)
  case StringLiteral(value: String)
  case Type(id: Types)
  case Storage(id: StorageTypes)
  case If(cond: Ast, ifTrue: Ast, orElse: Option[Ast])
  case Return(value: Option[Ast])
  case Assignment(lvalue: Identifier, rvalue: Ast)
  case Declaration(storage: StorageTypes, declType: Types, name: Identifier)
  case Function(storage: StorageTypes, returnType: Types, name: Identifier, args: ArgList, body: List[Ast])
  case Block(inner: List[Ast])
  case Legacy(inner: CAst)
}

object Ast {
  inline def storageOf(kind: StorageTypes): Storage = Storage(kind).asInstanceOf[Storage]
  inline def typeOf(kind: Types): Type = Type(kind).asInstanceOf[Type]
  inline def identifierOf(id: String): Identifier = Identifier(id).asInstanceOf[Identifier]
  // inline def blockOf(statements: List[Ast]): Block = Block(statements).asInstanceOf[Block]
}