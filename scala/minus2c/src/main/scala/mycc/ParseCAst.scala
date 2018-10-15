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

  private type Parse[T] = PartialFunction[CAst, T]

  def goal(ast: CAst): List[Declarations] = translationUnit(ast)

  private def translationUnit: Parse[List[Declarations]] =
    externalDeclarationList | externalDeclaration

  private val externalDeclarationList: Parse[List[Declarations]] = {
    case BinaryNode("E", list, decl) =>
      translationUnit(list).++[Declarations, List[Declarations]](externalDeclaration(decl))
  }

  private def externalDeclaration: Parse[List[Declarations]] =
    functionDefinition.L | declarationsAndAssignments

  private val functionDefinition: Parse[Function] = {
    case BinaryNode("D", declarators, UnaryNode("B", body)) => declarators match {
      case BinaryNode("d", types, declarator) =>
        val (storage, returnType) = declarationSpecifiersSpecific(types)
        val (name, args) = functionDeclarator(declarator)
        Function(storage, returnType, name, args, compoundStatements(body))
      case declarator => // warning implicit return type 'int'
        val (name, args) = functionDeclarator(declarator)
        Function(auto, int, name, args, compoundStatements(body))
    }
  }

  private val variableDeclaration: Parse[List[Declarations]] = {
    case BinaryNode("~", specifiers, expr) =>
      val (storage, declType) = declarationSpecifiersSpecific(specifiers)
      initDeclarators(expr).flatMap[Declarations, List[Declarations]] {
        case i: Identifier => Declaration(storage, declType, i) :: Nil
        case a @ Assignment(i, _) => Declaration(storage, declType, i) :: a :: Nil
      }
  }

  private def declarationsAndAssignments: Parse[List[Declarations]] =
    variableDeclaration |
    functionDefinition.L |
    `type`.E |
    storage.E

  private def expressionsStatement: Parse[Expressions] = empty | expressions
  private def jumpStatement: Parse[List[Statements]] = `return`.L

  private def compoundStatements: Parse[List[Statements]] =
    block.L |
    multiList |
    declarationsAndAssignments |
    expressionsStatement |
    jumpStatement |
    unimplementedError(value => s"statement: $value")

  private val block: Parse[Block] = {
    case UnaryNode("B", body) => Block(compoundStatements(body))
  }

  private val multiList: Parse[List[Statements]] = {
    case BinaryNode(";", front, end) =>
      compoundStatements(front) ++ compoundStatements(end)
  }

  private def expressions: Parse[Expressions] = expressionList | assignmentsAsExpressions
  private def assignments: Parse[Assignments] = assignment | equalities
  private def equalities: Parse[Equalities] = equality | relationals
  private def relationals: Parse[Relationals] = relational | additives
  private def additives: Parse[Additives] = additive | multiplicatives
  private def multiplicatives: Parse[Multiplicatives] = multiplicative | unaries
  private def unaries: Parse[Unaries] = unary | postfix
  private def postfix: Parse[Postfix] = application | primary
  private def primary: Parse[Primary] = identifier | constExpWrapperOrString

  private val assignment: Parse[Assignment] = {
    case BinaryNode("=", TokenString("id", id), value) => Assignment(Identifier(id), assignments(value))
  }

  private val equality: Parse[Equality] = {
    case BinaryNode("==", left, right) => Equality(EQUAL, equalities(left), relationals(right))
    case BinaryNode("!=", left, right) => Equality(NOT_EQUAL, equalities(left), relationals(right))
  }

  private val relational: Parse[Relational] = {
    case BinaryNode("<", left, right) => Relational(LT, relationals(left), additives(right))
    case BinaryNode(">", left, right) => Relational(GT, relationals(left), additives(right))
    case BinaryNode("<=", left, right) => Relational(LT_EQ, relationals(left), additives(right))
    case BinaryNode(">=", left, right) => Relational(GT_EQ, relationals(left), additives(right))
  }

  private val additive: Parse[Additive] = {
    case BinaryNode("+", left, right) => Additive(PLUS, additives(left), multiplicatives(right))
    case BinaryNode("-", left, right) => Additive(MINUS, additives(left), multiplicatives(right))
  }

  private val multiplicative: Parse[Multiplicative] = {
    case BinaryNode("*", left, right) => Multiplicative(MULTIPLY, multiplicatives(left), unaries(right))
    case BinaryNode("/", left, right) => Multiplicative(DIVIDE, multiplicatives(left), unaries(right))
    case BinaryNode("%", left, right) => Multiplicative(MODULUS, multiplicatives(left), unaries(right))
  }

  private val unary: Parse[Unary] = {
    case UnaryNode("&", unary) => Unary(REF, unaries(unary))
    case UnaryNode("*", unary) => Unary(POINTER_ACCESS, unaries(unary))
    case UnaryNode("+", unary) => Unary(POSTIVE, unaries(unary))
    case UnaryNode("-", unary) => Unary(NEGATIVE, unaries(unary))
    case UnaryNode("!", unary) => Unary(NOT, unaries(unary))
  }

  private val application: Parse[Application] = {
    case BinaryNode("apply", name, args) => throw UnimplementedError("Application with args list")
    case UnaryNode("apply", name) => Application(postfix(name), Nil)
  }

  private val identifier: Parse[Identifier] = {
    case TokenString("id", id) => Identifier(id)
  }

  private val constant: Parse[Constant] = {
    case TokenInt("constant", id) => Constant(id)
  }

  private val lazyExpressions: Parse[LazyExpressions] = {
    case UnaryNode("e", exprs) => LazyExpressions(expressions(exprs))
  }

  private val expressionList: Parse[Expressions] = {
    case BinaryNode(",", exprs, assigns) => expressions(exprs) :+ assignments(assigns)
  }

  private val stringLiteral: Parse[StringLiteral] = {
    case TokenString("string", value) => StringLiteral(value)
  }

  private val `return`: Parse[Return] = {
    case UnaryNode("return", value) => Return(expressions(value))
    case Singleton("return") => Return(Nil)
  }

  private val empty: Parse[Expressions] = {
    case Singleton("ø") => Nil
  }

  private def unimplementedError[E <: Ast](msg: Any => String): Parse[E] = {
    case value => throw UnimplementedError(msg(value))
  }

  private def unexpectedError[E <: Ast](msg: Any => String): Parse[E] = {
    case value => throw UnexpectedAstNode(msg(value))
  }

  private def semanticError[E <: Ast](msg: Any => String): Parse[E] = {
    case value => throw SemanticError(msg(value))
  }

  private def initDeclarators: Parse[List[InitDeclarator]] = initDeclaratorList | initDeclarator.L

  private def initDeclaratorList: Parse[List[InitDeclarator]] = {
    case BinaryNode(",", front, end) =>
      initDeclarators(front) :+ initDeclarator(end)
  }

  private def initDeclarator: Parse[InitDeclarator] = identifier | assignment

  private def declarationSpecifiersSpecific(specifiers: CAst): (StorageTypes, Types) = {
    val (storages, types) =
      declarationSpecifiers(specifiers).partition(_.isInstanceOf[Storage])

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

  private def declarationSpecifiers: Parse[List[DeclarationSpecifiers]] =
    declarationSpecifierList | declarationSpecifier.L

  private def declarationSpecifier: Parse[DeclarationSpecifiers] =
    `type` | storage

  private val declarationSpecifierList: Parse[List[DeclarationSpecifiers]] = {
    case BinaryNode("~", specifier, tail) =>
      declarationSpecifier(specifier) :: declarationSpecifiers(tail)
  }

  private val storage: Parse[Storage] = {
    case Singleton("extern") => Storage(extern)
    case Singleton("auto") => Storage(auto)
  }

  private val `type`: Parse[Type] = {
    case Singleton("int") => Type(int)
    case Singleton("function") => Type(function)
    case Singleton("void") => Type(void)
  }


  private def functionDeclarator: Parse[(Identifier, ArgList)] =
    directFunctionDeclarator | semanticError(_ => "No args on Function definition")

  private val directFunctionDeclarator: Parse[(Identifier, ArgList)] = {
    case UnaryNode("F", name) =>
      (identifier(name), LAny)
    case UnaryNode("V", name) =>
      (identifier(name), LVoid)
    case BinaryNode("F", name, args) =>
      (identifier(name), LParam(parameterList(args).toVector))
    case BinaryNode("H", _, _) =>
      throw UnimplementedError("Identifier only function parameter list not implemented.")
  }

  private def parameterList: Parse[List[(Types, Identifier)]] = {
    case BinaryNode(",", tail, param) =>
      parameterList(tail) :+ parameter(param)
    case param =>
      parameter(param) :: Nil
  }

  private def parameter: Parse[(Types, Identifier)] = {
    case BinaryNode("~", typeSpecifier, declarator) => declarator match {
      case BinaryNode("F", _, _) => throw UnimplementedError("C style function args, use 'function' type with identifier")
      case ident =>
        val identifierOnly = identifier | unexpectedError(value => "${value} is not an Identifier")
        val typeOnly = `type` | unexpectedError(value => "${value} is not a Type")
        (typeOnly(typeSpecifier).id, identifierOnly(ident))
    }
    case _ => throw SemanticError("not type and identifier")
  }

  private def constExpWrapperOrString: Parse[Primary] = constant | expWrapperOrString

  private def expWrapperOrString: Parse[Primary] = lazyExpressions | stringLiteral

  private def assignmentsAsExpressions: Parse[Expressions] = assignments.L
}