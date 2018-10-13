package object mycc {
  import CAst._
  import Ast._
  import Types._
  import ArgList._
  import StorageTypes._
  import mycc.exception._

  /**replace these to match expected values at each context level in C.y
   */
  def specialize(ast: CAst): Ast = ast match {
    case Singleton(value) => onSingleton(value)
    case TokenString(kind, lexeme) => onTokenString(kind, lexeme)
    case TokenInt(kind, value) => onTokenInt(kind, value)
    case UnaryNode(kind, a1) => onUnaryNode(kind, a1)
    case BinaryNode(kind, a1, a2) => onBinaryNode(kind, a1, a2)
  }

  def onSingleton(value: String): Ast = value match {
    case "int" => Type(int)
    case "void" => Type(void)
    case "function" => Type(function)
    case "ø" => ø
    case "return" => Return(None)
    case "auto" => Storage(auto)
    case "extern" => Storage(extern)
    case kind => Node0(kind)
  }

  def onTokenString(kind: String, lexeme: String): Ast = kind match {
    case "string" => StringLiteral(lexeme)
    case "id" => Identifier(lexeme)
    case _ => Leaf(kind, Left(lexeme))
  }

  def onTokenInt(kind: String, value: Int): Ast = kind match {
    case "constant" => Constant(value)
    case _ => Leaf(kind, Right(value))
  }

  def onUnaryNode(kind: String, a1: CAst): Ast = kind match {
    case "return" => Return(Some(specialize(a1)))
    case _ => Node1(kind, specialize(a1))
  }

  def onBinaryNode(kind: String, a1: CAst, a2: CAst): Ast = kind match {
    case "D" => matchFunctionDefinition(a1, a2)
    case "if" => matchIfElse(a1, a2)
    case _ => Node2(kind, specialize(a1), specialize(a2))
  }

  def matchFunctionDefinition(declarators: CAst, body: CAst): Ast = {
    val (decls, (name, args)) = matchDeclarators(declarators)
    Function(decls, name, args, matchCompoundStatement(body))
  }

  def matchIfElse(cond: CAst, bodyOrElse: CAst): Ast = bodyOrElse match {
    case BinaryNode("else", ifTrue, orElse) =>
      If(specialize(cond), specialize(ifTrue), Some(specialize(orElse)))
    case body =>
      If(specialize(cond), specialize(body), None)
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
    case typeAndIdentifier =>
      ListOf(Vector(matchTypeAndIdentifierAst(typeAndIdentifier)))
  }

  def matchCommaList(list: CAst): List[(Type, Identifier)] = list match {
    case BinaryNode(",", tail, typeAndIdentifier) =>
      matchTypeAndIdentifierAst(typeAndIdentifier) :: matchCommaList(tail)
    case typeAndIdentifier => matchTypeAndIdentifierAst(typeAndIdentifier) :: Nil
  }

  def matchTypeAndIdentifierAst(typeAndIdentifier: CAst): (Type, Identifier) = typeAndIdentifier match {
    case BinaryNode("~", typeSpecifier, declarator) => declarator match {
      case BinaryNode("F", _, _) => throw UnimplementedError("C style function args, use 'function' type with identifier")
      case identifier => matchTypeAndIdentifier(specialize(typeSpecifier), specialize(identifier))
    }
    case _ => throw SemanticError("not type and identifier")
  }

  def matchTypeAndIdentifier(typeAndIdentifier: (Ast, Ast)): (Type, Identifier) = typeAndIdentifier match {
    case t @ (_:Type, _:Identifier) => t.asInstanceOf[(Type, Identifier)]
    case _ => throw UnexpectedAstNode(s"${typeAndIdentifier} not of type ${(Type, Identifier)}")
  }

  def matchIdentifier(ast: Ast): Identifier = ast match {
    case i: Identifier => i
    case _ => throw UnexpectedAstNode("${typeAndIdentifier} not of type Identifier")
  }
}