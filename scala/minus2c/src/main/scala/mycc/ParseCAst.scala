package mycc

object ParseCAst {
  import CAst._
  import Ast._
  import Types._
  import ArgList._
  import StorageTypes._
  import EqualityOperators._
  import RelationalOperators._
  import AdditiveOperators._
  import MultiplicativeOperators._
  import UnaryOperators._
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

  private val parseFunctionDefinition: PartialFunction[CAst, Function] = {
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
    matchMultiList |
    matchDeclaration |
    matchExpressionsStatement |
    matchJumpStatement |
    unimplementedError(value => s"$value is not supported yet.")

  private val matchBlock: PartialFunction[CAst, Block] = {
    case UnaryNode("B", body) => Block(matchCompoundStatements(body))
  }

  private val matchMultiList: PartialFunction[CAst, List[Ast]] = {
    case BinaryNode(";", front, end) =>
      matchCompoundStatements(front) ++ matchCompoundStatements(end)
  }

  // private val matchExprList: PartialFunction[CAst, List[Ast]] = {
  //   case BinaryNode(",", front, end) => 
  //     val namesAndAssignments = matchNamesAndAssignments(front) ++ matchNamesAndAssignments(end)
  //     namesAndAssignments.map { (name, assignmentOp) =>
  //       assignmentOp.map {
  //         Assignment(name, _)
  //       } getOrElse {
  //         name
  //       }
  //     }
  // }

  private def matchAssignments: PartialFunction[CAst, Assignments] = matchAssignment | matchEqualities

  private def matchEqualities: PartialFunction[CAst, Equalities] = matchEquality | matchRelationals

  private def matchRelationals: PartialFunction[CAst, Relationals] = matchRelational | matchAdditives

  private def matchAdditives: PartialFunction[CAst, Additives] = matchAdditive | matchMultiplicatives

  private def matchMultiplicatives: PartialFunction[CAst, Multiplicatives] = matchMultiplicative | matchUnaries

  private def matchUnaries: PartialFunction[CAst, Unaries] = matchUnary | matchPostfix

  private def matchPostfix: PartialFunction[CAst, Postfix] = matchApplication | matchPrimary
  
  private def matchPrimary: PartialFunction[CAst, Primary] = matchIdentifier | matchConstExpWrapperOrString

  private def matchConstExpWrapperOrString: PartialFunction[CAst, Primary] = matchConstant | matchExpWrapperOrString

  private def matchExpWrapperOrString: PartialFunction[CAst, Primary] = matchLazyExpressions | matchStringLiteral

  private val matchAssignment: PartialFunction[CAst, Assignment] = {
    case BinaryNode("=", TokenString("id", id), value) => Assignment(identifierOf(id), matchAssignments(value))
  }

  private val matchEquality: PartialFunction[CAst, Equality] = {
    case BinaryNode("==", left, right) => Equality(EQUAL, matchEqualities(left), matchRelationals(right))
    case BinaryNode("!=", left, right) => Equality(NOT_EQUAL, matchEqualities(left), matchRelationals(right))
  }

  private val matchRelational: PartialFunction[CAst, Relational] = {
    case BinaryNode("<", left, right) => Relational(LT, matchRelationals(left), matchAdditives(right))
    case BinaryNode(">", left, right) => Relational(GT, matchRelationals(left), matchAdditives(right))
    case BinaryNode("<=", left, right) => Relational(LT_EQ, matchRelationals(left), matchAdditives(right))
    case BinaryNode(">=", left, right) => Relational(GT_EQ, matchRelationals(left), matchAdditives(right))
  }

  private val matchAdditive: PartialFunction[CAst, Additive] = {
    case BinaryNode("+", left, right) => Additive(PLUS, matchAdditives(left), matchMultiplicatives(right))
    case BinaryNode("-", left, right) => Additive(MINUS, matchAdditives(left), matchMultiplicatives(right))
  }

  private val matchMultiplicative: PartialFunction[CAst, Multiplicative] = {
    case BinaryNode("*", left, right) => Multiplicative(MULTIPLY, matchMultiplicatives(left), matchUnaries(right))
    case BinaryNode("/", left, right) => Multiplicative(DIVIDE, matchMultiplicatives(left), matchUnaries(right))
  }

  private val matchUnary: PartialFunction[CAst, Unary] = {
    case UnaryNode("&", unary) => Unary(REF, matchUnaries(unary))
    case UnaryNode("*", unary) => Unary(POINTER_ACCESS, matchUnaries(unary))
    case UnaryNode("+", unary) => Unary(POSTIVE, matchUnaries(unary))
    case UnaryNode("-", unary) => Unary(NEGATIVE, matchUnaries(unary))
    case UnaryNode("!", unary) => Unary(NOT, matchUnaries(unary))
  }

  private val matchApplication: PartialFunction[CAst, Application] = {
    case BinaryNode("apply", name, args) => throw UnimplementedError("Application with args list")
    case UnaryNode("apply", name) => Application(matchPostfix(name), Nil)
  }

  private val matchIdentifier: PartialFunction[CAst, Identifier] = {
    case TokenString("id", id) => Identifier(id)
  }

  private val matchConstant: PartialFunction[CAst, Constant] = {
    case TokenInt("constant", id) => Constant(id)
  }

  private val matchLazyExpressions: PartialFunction[CAst, LazyExpressions] = {
    case UnaryNode("e", expressions) => LazyExpressions(matchExpressions(expressions))
  }

  private def matchExpressionsStatement: PartialFunction[CAst, Expressions] = matchEmpty | matchExpressions

  private def matchExpressions: PartialFunction[CAst, Expressions] = matchAssignmentsAsExpressions | matchExpressionList

  private def matchAssignmentsAsExpressions: PartialFunction[CAst, Expressions] = matchAssignments.L

  private val matchExpressionList: PartialFunction[CAst, Expressions] = {
    case BinaryNode(",", expressions, assignments) => matchExpressions(expressions) :+ matchAssignments(assignments)
  }

  private val matchStringLiteral: PartialFunction[CAst, StringLiteral] = {
    case TokenString("string", value) => StringLiteral(value)
  }

  private def matchJumpStatement: PartialFunction[CAst, List[Ast]] = matchReturn

  private val matchReturn: PartialFunction[CAst, List[Ast]] = {
    case UnaryNode("return", value) => matchCompoundStatements(value) match {
      case Nil => throw UnexpectedAstNode("expected non empty list")
      case expr :: Nil => Return(Some(expr)) :: Nil
      case list => list.init :+ Return(Some(list.last))
    }
  }

  private val matchEmpty: PartialFunction[CAst, Expressions] = {
    case Singleton("ø") => Nil
  }

  def unimplementedError[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
    case value => throw UnimplementedError(msg(value))
  }

  def unexpectedError[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
    case value => throw UnexpectedAstNode(msg(value))
  }

  def semanticError[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
    case value => throw SemanticError(msg(value))
  }

  def matchNamesAndAssignments(expr: CAst): List[(Identifier, Option[Ast])] = expr match {
    case TokenString("id", id) => List((identifierOf(id), None))
    case BinaryNode("=", TokenString("id", id), value) => List((identifierOf(id), Some(matchAssignments(value))))
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
      case "extern" => Storage(extern)
      case "auto" => Storage(auto)
    }
  }

  private val matchType: PartialFunction[CAst, Type] = {
    case Singleton(kind) => kind match {
      case "int" => Type(int)
      case "function" => Type(function)
      case "void" => Type(void)
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
}