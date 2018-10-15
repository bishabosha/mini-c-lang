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

  def matchTranslationUnit: PartialFunction[CAst, List[Declarations]] =
    matchExternalDeclarationList | matchExternalDeclaration

  private val matchExternalDeclarationList: PartialFunction[CAst, List[Declarations]] = {
    case BinaryNode("E", list, externalDeclaration) =>
      matchTranslationUnit(list).++[Declarations, List[Declarations]](matchExternalDeclaration(externalDeclaration))
  }

  private def matchExternalDeclaration: PartialFunction[CAst, List[Declarations]] =
    matchFunctionDefinition.L | matchDeclarationsAndAssignments

  private val matchFunctionDefinition: PartialFunction[CAst, Function] = {
    case BinaryNode("D", declarators, UnaryNode("B", body)) => declarators match {
      case BinaryNode("d", types, functionDeclarator) =>
        val (storage, returnType) = matchDeclarationSpecifiersSpecific(types)
        val (name, args) = matchFunctionDeclarator(functionDeclarator)
        Function(storage, returnType, name, args, matchCompoundStatements(body))
      case functionDeclarator => // warning implicit return type 'int'
        val (name, args) = matchFunctionDeclarator(functionDeclarator)
        Function(auto, int, name, args, matchCompoundStatements(body))
    }
  }

  private val matchVariableDeclaration: PartialFunction[CAst, List[Declarations]] = {
    case BinaryNode("~", specifiers, expr) =>
      val (storage, declType) = matchDeclarationSpecifiersSpecific(specifiers)
      matchInitDeclarators(expr).flatMap[Declarations, List[Declarations]] {
        case i: Identifier => Declaration(storage, declType, i) :: Nil
        case a @ Assignment(i, _) => Declaration(storage, declType, i) :: a :: Nil
      }
  }

  private def matchDeclarationsAndAssignments: PartialFunction[CAst, List[Declarations]] =
    matchVariableDeclaration |
    matchFunctionDefinition.L |
    matchType.E |
    matchStorage.E

  private def matchExpressionsStatement: PartialFunction[CAst, Expressions] = matchEmpty | matchExpressions
  private def matchJumpStatement: PartialFunction[CAst, List[Statements]] = matchReturn.L

  private def matchCompoundStatements: PartialFunction[CAst, List[Statements]] =
    matchBlock.L |
    matchMultiList |
    matchDeclarationsAndAssignments |
    matchExpressionsStatement |
    matchJumpStatement |
    unimplementedError(value => s"statement: $value")

  private val matchBlock: PartialFunction[CAst, Block] = {
    case UnaryNode("B", body) => Block(matchCompoundStatements(body))
  }

  private val matchMultiList: PartialFunction[CAst, List[Statements]] = {
    case BinaryNode(";", front, end) =>
      matchCompoundStatements(front) ++ matchCompoundStatements(end)
  }

  private def matchExpressions: PartialFunction[CAst, Expressions] = matchExpressionList | matchAssignmentsAsExpressions
  private def matchAssignments: PartialFunction[CAst, Assignments] = matchAssignment | matchEqualities
  private def matchEqualities: PartialFunction[CAst, Equalities] = matchEquality | matchRelationals
  private def matchRelationals: PartialFunction[CAst, Relationals] = matchRelational | matchAdditives
  private def matchAdditives: PartialFunction[CAst, Additives] = matchAdditive | matchMultiplicatives
  private def matchMultiplicatives: PartialFunction[CAst, Multiplicatives] = matchMultiplicative | matchUnaries
  private def matchUnaries: PartialFunction[CAst, Unaries] = matchUnary | matchPostfix
  private def matchPostfix: PartialFunction[CAst, Postfix] = matchApplication | matchPrimary
  private def matchPrimary: PartialFunction[CAst, Primary] = matchIdentifier | matchConstExpWrapperOrString

  private val matchAssignment: PartialFunction[CAst, Assignment] = {
    case BinaryNode("=", TokenString("id", id), value) => Assignment(Identifier(id), matchAssignments(value))
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
    case BinaryNode("%", left, right) => Multiplicative(MODULUS, matchMultiplicatives(left), matchUnaries(right))
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

  private val matchExpressionList: PartialFunction[CAst, Expressions] = {
    case BinaryNode(",", expressions, assignments) => matchExpressions(expressions) :+ matchAssignments(assignments)
  }

  private val matchStringLiteral: PartialFunction[CAst, StringLiteral] = {
    case TokenString("string", value) => StringLiteral(value)
  }

  private val matchReturn: PartialFunction[CAst, Return] = {
    case UnaryNode("return", value) => Return(matchExpressions(value))
    case Singleton("return") => Return(Nil)
  }

  private val matchEmpty: PartialFunction[CAst, Expressions] = {
    case Singleton("ø") => Nil
  }

  private def unimplementedError[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
    case value => throw UnimplementedError(msg(value))
  }

  private def unexpectedError[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
    case value => throw UnexpectedAstNode(msg(value))
  }

  private def semanticError[I, E <: Ast](msg: I => String): PartialFunction[I, E] = {
    case value => throw SemanticError(msg(value))
  }

  def matchInitDeclarators: PartialFunction[CAst, List[InitDeclarator]] = matchInitDeclaratorList | matchInitDeclarator.L

  def matchInitDeclaratorList: PartialFunction[CAst, List[InitDeclarator]] = {
    case BinaryNode(",", front, end) =>
      matchInitDeclarators(front) :+ matchInitDeclarator(end)
  }

  def matchInitDeclarator: PartialFunction[CAst, InitDeclarator] = matchIdentifier | matchAssignment

  private def matchDeclarationSpecifiersSpecific(specifiers: CAst): (StorageTypes, Types) = {
    val (storages, types) =
      matchDeclarationSpecifiers(specifiers).partition(_.isInstanceOf[Storage])

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

  private def matchDeclarationSpecifiers: PartialFunction[CAst, List[DeclarationSpecifiers]] =
    matchDeclarationSpecifierList | matchDeclarationSpecifier.L

  private def matchDeclarationSpecifier: PartialFunction[CAst, DeclarationSpecifiers] =
    matchType | matchStorage

  private val matchDeclarationSpecifierList: PartialFunction[CAst, List[DeclarationSpecifiers]] = {
    case BinaryNode("~", specifier, tail) =>
      matchDeclarationSpecifier(specifier) :: matchDeclarationSpecifiers(tail)
  }

  private val matchStorage: PartialFunction[CAst, Storage] = {
    case Singleton("extern") => Storage(extern)
    case Singleton("auto") => Storage(auto)
  }

  private val matchType: PartialFunction[CAst, Type] = {
    case Singleton("int") => Type(int)
    case Singleton("function") => Type(function)
    case Singleton("void") => Type(void)
  }


  private def matchFunctionDeclarator: PartialFunction[CAst, (Identifier, ArgList)] =
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

  private def matchParameterList: PartialFunction[CAst, List[(Types, Identifier)]] = {
    case BinaryNode(",", tail, parameter) =>
      matchParameterList(tail) :+ matchParameter(parameter)
    case parameter =>
      matchParameter(parameter) :: Nil
  }

  private def matchParameter: PartialFunction[CAst, (Types, Identifier)] = {
    case BinaryNode("~", typeSpecifier, declarator) => declarator match {
      case BinaryNode("F", _, _) => throw UnimplementedError("C style function args, use 'function' type with identifier")
      case identifier =>
        val identifierOnly = matchIdentifier | unexpectedError(value => "${value} is not an Identifier")
        val typeOnly = matchType | unexpectedError(value => "${value} is not a Type")
        (typeOnly(typeSpecifier).id, identifierOnly(identifier))
    }
    case _ => throw SemanticError("not type and identifier")
  }

  private def matchConstExpWrapperOrString: PartialFunction[CAst, Primary] = matchConstant | matchExpWrapperOrString

  private def matchExpWrapperOrString: PartialFunction[CAst, Primary] = matchLazyExpressions | matchStringLiteral

  private def matchAssignmentsAsExpressions: PartialFunction[CAst, Expressions] = matchAssignments.L
}