package object mycc {
  import CAst._
  import Ast._
  import Types._
  import ArgList._
  import StorageTypes._
  import mycc.exception._

  def specialize(ast: CAst): Ast = ast match {
    case Singleton("int") =>
      Type(int)

    case Singleton("void") =>
      Type(void)

    case Singleton("function") =>
      Type(function)

    case Singleton("ø") =>
      ø

    case Singleton("return") =>
      Return(None)

    case Singleton("auto") =>
      Storage(auto)

    case Singleton("extern") =>
      Storage(extern)

    case Singleton(kind) =>
      Node0(kind)

    case TokenString("string", value) =>
      StringLiteral(value)

    case TokenString("id", identifier) =>
      Identifier(identifier)

    case TokenString(kind, data) =>
      Leaf(kind, Left(data))

    case TokenInt("constant", value) =>
      Constant(value)

    case TokenInt(kind, data) =>
      Leaf(kind, Right(data))

    case UnaryNode("return", value) =>
      Return(Some(specialize(value)))

    case UnaryNode(kind, left) =>
      Node1(kind, specialize(left))

    case BinaryNode("D", declarators, body) =>
      val (decls, (name, args)) = matchDeclarators(declarators)
      Function(decls, name, args, matchCompoundStatement(body))

    case BinaryNode("if", cond, tail) =>
      tail match {
        case BinaryNode("else", ifTrue, orElse) =>
          If(specialize(cond), specialize(ifTrue), Some(specialize(orElse)))
        case _ =>
          If(specialize(cond), specialize(tail), None)
      }

    case BinaryNode(kind, left, right) =>
      Node2(kind, specialize(left), specialize(right))
  }
  
  def matchCompoundStatement(compoundStatement: CAst): Ast = compoundStatement match {
    case _ => specialize(compoundStatement)
  }

  def matchDeclarationSpecifiers(specifiers: CAst): Vector[Type | Storage] =
    matchTildaList(specifiers).toVector

  def matchTildaList(list: CAst): List[Type | Storage] = list match {
    case BinaryNode("~", specifier, tail) =>
      matchStorageOrType(specialize(specifier)) :: matchTildaList(tail)
    case single => matchStorageOrType(specialize(single)) :: Nil
  }

  def matchStorageOrType(value: Ast): Type | Storage = value match {
    case s: Storage => s
    case t: Type => t
    case _ => throw SemanticError("Not Storage or Type")
  }

  def matchDeclarators(declarators: CAst): (Vector[Type | Storage], (Identifier, ArgList)) = declarators match {
    case BinaryNode("d", types, nameAndArgs) =>
      (matchDeclarationSpecifiers(types), matchNameAndArgs(nameAndArgs))
    case nameAndArgs =>
      (Vector(), matchNameAndArgs(nameAndArgs))
  }

  def matchNameAndArgs(nameAndArgs: CAst): (Identifier, ArgList) = nameAndArgs match {
    case UnaryNode("F", name) =>
      (matchIdentifier(specialize(name)), ListOf(Vector()))
    case BinaryNode("F", name, args) =>
      (matchIdentifier(specialize(name)), matchArgsList(args))
    case BinaryNode("H", _, _) =>
      throw UnimplementedError("Identifier only function parameter list not implemented.")
    case _ => 
      throw SemanticError("No args on Function definition")
  }

  def matchArgsList(args: CAst): ArgList = args match {
    case Singleton("void") => LVoid
    case BinaryNode(",", tail, typeAndIdentifier) =>
      ListOf((matchTypeAndIdentifierAst(typeAndIdentifier) :: matchCommaList(tail)).reverse.toVector)
    case BinaryNode("~", typeSpecifier, identifier) =>
      ListOf(Vector(matchTypeAndIdentifier(specialize(typeSpecifier), specialize(identifier))))
    case _ => throw SemanticError("not args list")
  }

  def matchCommaList(list: CAst): List[(Type, Identifier)] = list match {
    case BinaryNode(",", tail, typeAndIdentifier) =>
      matchTypeAndIdentifierAst(typeAndIdentifier) :: matchCommaList(tail)
    case typeAndIdentifier => matchTypeAndIdentifierAst(typeAndIdentifier) :: Nil
  }

  def matchTypeAndIdentifierAst(typeAndIdentifier: CAst): (Type, Identifier) = typeAndIdentifier match {
    case BinaryNode("~", typeSpecifier, identifier) => matchTypeAndIdentifier(specialize(typeSpecifier), specialize(identifier))
    case _ => throw SemanticError("not type and identifier")
  }

  def matchTypeAndIdentifier(typeAndIdentifier: (Ast, Ast)): (Type, Identifier) = typeAndIdentifier match {
    case t @ (_:Type, _:Identifier) => t.asInstanceOf[(Type, Identifier)]
    case _ => throw SemanticError("not type and identifier")
  }

  def matchIdentifier(ast: Ast): Identifier = ast match {
    case i: Identifier => i
    case _ => throw SemanticError("Not Identifier")
  }
}