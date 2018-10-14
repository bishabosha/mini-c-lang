package mycc

object ParseCAst {
  import CAst._
  import Ast._
  import Types._
  import ArgList._
  import StorageTypes._
  import mycc.exception._

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
    val (storage, returnType, (name, args)) = matchDeclarators(declarators)
    Function(storage, returnType, name, args, matchBlock(body))
  }
  
  def matchBlock(compoundStatement: CAst): Block = compoundStatement match {
    case UnaryNode("B", body) => blockOf(matchStatementsInReverse(body).reverse)
    case _ => ???
  }

  def matchStatementsInReverse(statement: CAst): List[Ast] = statement match {
    case BinaryNode(";", front, end) =>
      matchStatementsInReverse(end) ++ matchStatementsInReverse(front)

    case BinaryNode("~", specifiers, expr) =>
      val (storage, declType) = matchDeclarationSpecifiers(specifiers)
      val namesAndAssignments = matchNamesAndAssignments(expr)

      namesAndAssignments.reverse.flatMap { (name, assignmentOp) =>
        assignmentOp.map {
          Assignment(name, _) :: Declaration(storage, declType, name) :: Nil
        } getOrElse {
          Declaration(storage, declType, name) :: Nil
        }
      }
      
    case BinaryNode(",", front, end) => 
      val namesAndAssignments = matchNamesAndAssignments(front) ++ matchNamesAndAssignments(end)
      namesAndAssignments.reverse.flatMap { (name, assignmentOp) =>
        assignmentOp.map {
          Assignment(name, _) :: Nil
        } getOrElse {
          name :: Nil
        }
      }

    case BinaryNode("=", TokenString("id", id), value) =>
      Assignment(identifierOf(id), specialize(value)) :: Nil

    case TokenString("id", id) =>
      Identifier(id) :: Nil

    case UnaryNode("return", value) => matchStatementsInReverse(value) match {
      case Nil => throw UnexpectedAstNode("expected non empty list")
      case expr :: Nil => Return(Some(expr)) :: Nil
      case list => Return(Some(list.head)) :: list.tail
    }

    case _ => specialize(statement) :: Nil
  }

  def matchNamesAndAssignments(expr: CAst): List[(Identifier, Option[Ast])] = expr match {
    case TokenString("id", id) => List((identifierOf(id), None))
    case BinaryNode("=", TokenString("id", id), value) => List((identifierOf(id), Some(specialize(value))))
    case BinaryNode(",", front, end) =>
      matchNamesAndAssignments(front) ++ matchNamesAndAssignments(end)
    case _ => ???
  }

  def matchDeclarationSpecifiers(specifiers: CAst): (StorageTypes, Types) = {
    val (storages, types) = matchTildaList(specifiers)
      .partition(_.isInstanceOf[Storage])

    val storage: StorageTypes = storages match {
      case Nil => auto
      case (s: Storage) :: Nil => s.id
      case _ => throw SemanticError("More than one storage class may not be specified.")
    }

    val returnType: Types = types match {
      case Nil => int // warning implicit return type 'int'
      case (t: Type) :: Nil => t.id
      case _ => throw SemanticError("Invalid combination of type specifiers.")
    }

    (storage, returnType)
  }

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

  def matchDeclarators(declarators: CAst): (StorageTypes, Types, (Identifier, ArgList)) = declarators match {
    case BinaryNode("d", types, nameAndArgs) =>
      val (storage, returnType) = matchDeclarationSpecifiers(types)
      (storage, returnType, matchNameAndArgs(nameAndArgs))
    case nameAndArgs => // warning implicit return type 'int'
      (auto, int, matchNameAndArgs(nameAndArgs))
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

  def matchIfElse(cond: CAst, bodyOrElse: CAst): Ast = bodyOrElse match {
    case BinaryNode("else", ifTrue, orElse) =>
      If(specialize(cond), specialize(ifTrue), Some(specialize(orElse)))
    case body =>
      If(specialize(cond), specialize(body), None)
  }
}