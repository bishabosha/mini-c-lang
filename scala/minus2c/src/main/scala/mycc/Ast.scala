package mycc

import Ast._

enum Types {
  case int, void, function
}

enum EqualityOperators {
  case EQUAL, NOT_EQUAL
}

enum RelationalOperators {
  case LT, GT, LT_EQ, GT_EQ
}

enum AdditiveOperators {
  case PLUS, MINUS
}

enum MultiplicativeOperators {
  case MULTIPLY, DIVIDE
}

enum UnaryOperators {
  case REF, NOT, POSTIVE, NEGATIVE, POINTER_ACCESS
}

enum StorageTypes {
  case auto, extern
}

enum ArgList {
  case LVoid, LAny
  case LParam(list: Vector[(Types, Identifier)])
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
  case Application(operand: Postfix, args: List[Ast])
  case Unary(op: UnaryOperators, value: Unaries)
  case Multiplicative(op: MultiplicativeOperators, left: Multiplicatives, right: Unaries)
  case Additive(op: AdditiveOperators, left: Additives, right: Multiplicatives)
  case Relational(op: RelationalOperators, left: Relationals, right: Additives)
  case Equality(op: EqualityOperators, left: Equalities, right: Relationals)
  case Assignment(lvalue: Identifier, rvalue: Ast)
  case Declaration(storage: StorageTypes, declType: Types, name: Identifier)
  case Function(storage: StorageTypes, returnType: Types, name: Identifier, args: ArgList, body: List[Ast])
  case Block(inner: List[Ast])
  case Legacy(inner: CAst)
}

object Ast {
  type Primary = Identifier | Constant | StringLiteral
  type Postfix = Primary | Application
  type Unaries = Postfix | Unary
  type Multiplicatives = Unaries | Multiplicative
  type Additives = Multiplicatives | Additive
  type Relationals = Additives | Relational
  type Equalities = Relationals | Equality
  inline def storageOf(kind: StorageTypes): Storage = Storage(kind).asInstanceOf[Storage]
  inline def typeOf(kind: Types): Type = Type(kind).asInstanceOf[Type]
  inline def identifierOf(id: String): Identifier = Identifier(id).asInstanceOf[Identifier]
  // inline def blockOf(statements: List[Ast]): Block = Block(statements).asInstanceOf[Block]
}