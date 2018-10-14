package mycc

object ParseCAst {
  import CAst._
  import Ast._
  import Types._
  import ArgList._
  import StorageTypes._
  import mycc.exception._
  import PartialFunctionConversions._

  def parseTranslationUnit(ast: CAst): List[Ast] = ast match {
    case BinaryNode("E", list, externalDeclaration) =>
      parseTranslationUnit(list) ++ parseExternalDeclaration(externalDeclaration)
    case externalDeclaration =>
      parseExternalDeclaration(externalDeclaration)
  } 

  def parseExternalDeclaration: PartialFunction[CAst, List[Ast]] =
    parseFunctionDefinition.L | matchDeclaration

  private val parseFunctionDefinition: PartialFunction[CAst, Ast] = {
    case BinaryNode("D", declarators, UnaryNode("B", body)) => declarators match {
      case BinaryNode("d", types, functionDeclarator) =>
        val (storage, returnType) = matchDeclarationSpecifiers(types)
        val (name, args) = matchFunctionDeclarator(functionDeclarator)
        Function(storage, returnType, name, args, matchCompoundStatements(body))
      case functionDeclarator => // warning implicit return type 'int'
        val (name, args) = matchFunctionDeclarator(functionDeclarator)
        Function(auto, int, name, args, matchCompoundStatements(body))
    }
  }

  private val matchDeclaration: PartialFunction[CAst, List[Ast]] = {
    case BinaryNode("~", specifiers, expr) =>
      val (storage, declType) = matchDeclarationSpecifiers(specifiers)
      val namesAndAssignments = matchNamesAndAssignments(expr)

      namesAndAssignments.flatMap { (name, assignmentOp) =>
        assignmentOp.map {
          Declaration(storage, declType, name) :: Assignment(name, _) :: Nil
        } getOrElse {
          Declaration(storage, declType, name) :: Nil
        }
      }
  }

  def matchCompoundStatements: PartialFunction[CAst, List[Ast]] =
    matchBlock.L |
    matchListStatements |
    matchDeclaration |
    matchExprList |
    matchAssignment.L |
    matchIdentifier.L |
    matchReturn |
    matchDefault.L

  private val matchBlock: PartialFunction[CAst, Ast] = {
    case UnaryNode("B", body) => Block(matchCompoundStatements(body))
  }

  private val matchListStatements: PartialFunction[CAst, List[Ast]] = {
    case BinaryNode(";", front, end) =>
      matchCompoundStatements(front) ++ matchCompoundStatements(end)
  }

  private val matchExprList: PartialFunction[CAst, List[Ast]] = {
    case BinaryNode(",", front, end) => 
      val namesAndAssignments = matchNamesAndAssignments(front) ++ matchNamesAndAssignments(end)
      namesAndAssignments.map { (name, assignmentOp) =>
        assignmentOp.map {
          Assignment(name, _)
        } getOrElse {
          name
        }
      }
  }

  private val matchAssignment: PartialFunction[CAst, Ast] = {
    case BinaryNode("=", TokenString("id", id), value) => Assignment(identifierOf(id), specialize(value))
  }

  private val matchIdentifier: PartialFunction[CAst, Identifier] = {
    case TokenString("id", id) => Identifier(id).asInstanceOf[Identifier]
  }

  private val matchReturn: PartialFunction[CAst, List[Ast]] = {
    case UnaryNode("return", value) => matchCompoundStatements(value) match {
      case Nil => throw UnexpectedAstNode("expected non empty list")
      case expr :: Nil => Return(Some(expr)) :: Nil
      case list => list.init :+ Return(Some(list.last))
    }
  }

  private val matchDefault: PartialFunction[CAst, Ast] = {
    case ast => specialize(ast)
  }

  // def unimplemented[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
  //   case value => throw UnimplementedError(msg(value))
  // }

  def unexpectedError[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
    case value => throw UnexpectedAstNode(msg(value))
  }

  def semanticError[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
    case value => throw SemanticError(msg(value))
  }

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
    case kind => Legacy(Singleton(value))
  }

  def onTokenString(kind: String, lexeme: String): Ast = kind match {
    case "string" => StringLiteral(lexeme)
    case "id" => Identifier(lexeme)
    case _ => Legacy(TokenString(kind, lexeme))
  }

  def onTokenInt(kind: String, value: Int): Ast = kind match {
    case "constant" => Constant(value)
    case _ => Legacy(TokenInt(kind, value))
  }

  def onUnaryNode(kind: String, a1: CAst): Ast = kind match {
    case "return" => Return(Some(specialize(a1)))
    case _ => Legacy(UnaryNode(kind, a1))
  }

  def onBinaryNode(kind: String, a1: CAst, a2: CAst): Ast = kind match {
    case "if" => matchIfElse(a1, a2)
    case _ => Legacy(BinaryNode(kind, a1, a2))
  }

  def matchNamesAndAssignments(expr: CAst): List[(Identifier, Option[Ast])] = expr match {
    case TokenString("id", id) => List((identifierOf(id), None))
    case BinaryNode("=", TokenString("id", id), value) => List((identifierOf(id), Some(specialize(value))))
    case BinaryNode(",", front, end) =>
      matchNamesAndAssignments(front) ++ matchNamesAndAssignments(end)
    case _ => ???
  }

  def matchDeclarationSpecifiers(specifiers: CAst): (StorageTypes, Types) = {
    val (storages, types) =
      matchDeclarationSpecifierList(specifiers).partition(_.isInstanceOf[Storage])

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

  def matchDeclarationSpecifierList(list: CAst): List[Type | Storage] = list match {
    case BinaryNode("~", specifier, tail) =>
      matchDeclarationSpecifier(specifier) :: matchDeclarationSpecifierList(tail)
    case single => matchDeclarationSpecifier(single) :: Nil
  }

  private val matchStorage: PartialFunction[CAst, Storage] = {
    case Singleton(kind) => kind match {
      case "extern" => Storage(extern).asInstanceOf[Storage]
      case "auto" => Storage(auto).asInstanceOf[Storage]
    }
  }

  private val matchType: PartialFunction[CAst, Type] = {
    case Singleton(kind) => kind match {
      case "int" => Type(int).asInstanceOf[Type]
      case "function" => Type(function).asInstanceOf[Type]
      case "void" => Type(void).asInstanceOf[Type]
    }
  }

  def matchDeclarationSpecifier: PartialFunction[CAst, Type | Storage] = matchType | matchStorage

  def matchFunctionDeclarator: PartialFunction[CAst, (Identifier, ArgList)] =
    matchDirectFunctionDeclarator | semanticError(_ => "No args on Function definition")

  private val matchDirectFunctionDeclarator: PartialFunction[CAst, (Identifier, ArgList)] = {
    case UnaryNode("F", name) =>
      (matchIdentifier(name), LAny)
    case UnaryNode("V", name) =>
      (matchIdentifier(name), LVoid)
    case BinaryNode("F", name, args) =>
      (matchIdentifier(name), LParam(matchParameterList(args).toVector))
    case BinaryNode("H", _, _) =>
      throw UnimplementedError("Identifier only function parameter list not implemented.")
  }

  def matchParameterList: PartialFunction[CAst, List[(Types, Identifier)]] = {
    case BinaryNode(",", tail, parameter) =>
      matchParameterList(tail) :+ matchParameter(parameter)
    case parameter =>
      matchParameter(parameter) :: Nil
  }

  def matchParameter(parameter: CAst): (Types, Identifier) = parameter match {
    case BinaryNode("~", typeSpecifier, declarator) => declarator match {
      case BinaryNode("F", _, _) => throw UnimplementedError("C style function args, use 'function' type with identifier")
      case identifier =>
        val identifierOnly = matchIdentifier | unexpectedError(value => "${value} is not an Identifier")
        val typeOnly = matchType | unexpectedError(value => "${value} is not a Type")
        (typeOnly(typeSpecifier).id, identifierOnly(identifier))
    }
    case _ => throw SemanticError("not type and identifier")
  }

  def matchIfElse(cond: CAst, bodyOrElse: CAst): Ast = bodyOrElse match {
    case BinaryNode("else", ifTrue, orElse) =>
      If(specialize(cond), specialize(ifTrue), Some(specialize(orElse)))
    case body =>
      If(specialize(cond), specialize(body), None)
  }
}