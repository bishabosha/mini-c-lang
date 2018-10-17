package mycc

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
import ParseCAst._

object ParseCAst {
  private type Parse[T] = PartialFunction[CAst, T]

  type Context = Unit
  type Goal = List[Declarations]

  def apply(ast: CAst): (Context, Goal) = new ParseCAst().goal(ast)
}

class ParseCAst {

  private var context: Context = ()

  private def goal: Parse[(Context, Goal)] =
    translationUnit ->> { goal => context -> goal }

  private def translationUnit: Parse[Goal] =
    externalDeclarationList |
    externalDeclaration
    
  private def externalDeclaration: Parse[List[Declarations]] =
    functionDefinition.L |
    declarationsAndAssignments

  private def declarationsAndAssignments: Parse[List[Declarations]] =
    variableDeclaration |
    functionDefinition.L |
    declarationSpecifier.E

  private def declarationSpecifiers: Parse[List[DeclarationSpecifiers]] =
    declarationSpecifierList |
    declarationSpecifier.L

  private def declarationSpecifier: Parse[DeclarationSpecifiers] =
    `type` |
    storage

  private def declarationSpecifiersSpecific: Parse[(StorageTypes, Types)] =
    declarationSpecifiers ->> reduceDeclarationSpecifiers

  private def jumpStatement: Parse[List[Statements]] =
    `return`.L

  private def expressionsStatement: Parse[Expressions] =
    empty |
    expressions

  private def expressions: Parse[Expressions] =
    expressionList |
    assignmentsAsExpressions

  private def assignmentsAsExpressions: Parse[Expressions] =
    assignments.L

  private def assignments: Parse[Assignments] =
    assignment |
    equalities

  private def equalities: Parse[Equalities] =
    equality |
    relationals

  private def relationals: Parse[Relationals] =
    relational |
    additives

  private def additives: Parse[Additives] =
    additive |
    multiplicatives

  private def multiplicatives: Parse[Multiplicatives] =
    multiplicative |
    unaries

  private def unaries: Parse[Unaries] =
    unary |
    postfix

  private def postfix: Parse[Postfix] =
    application |
    primary

  private def primary: Parse[Primary] =
    identifier |
    (constant |
      (lazyExpressions |
        stringLiteral))

  private def initDeclarators: Parse[List[InitDeclarator]] =
    initDeclaratorList |
    initDeclarator.L

  private def initDeclarator: Parse[InitDeclarator] =
    declarator |
    assignment 

  private def declarator: Parse[InitDeclarator] =
    identifier |
    functionDeclarator

  private def functionDeclarator: Parse[FunctionDeclarator] =
    directFunctionDeclarator

  private def types: Parse[Types] =
    `type` ->> { _.id }

  private def parameters: Parse[List[Parameter]] =
    parameterList |
    parameter.L

  private def parameter: Parse[Parameter] =
    typesAndIdentifier |
    (types |
      identifier ->> { int -> })

  private def compoundStatements: Parse[List[Statements]] =
    block.L |
    multiList |
    declarationsAndAssignments |
    expressionsStatement |
    jumpStatement |
    { case value => throw UnimplementedError(s"statement: $value") }

  private val externalDeclarationList: Parse[List[Declarations]] = {
    case BinaryNode("E", list, decl) =>
      translationUnit(list).++[Declarations, List[Declarations]](externalDeclaration(decl))
  }

  private val functionDefinition: Parse[Function] = {
    case BinaryNode("D", declarators, UnaryNode("B", body)) => declarators match {
      case BinaryNode("d", types, declarator) =>
        val (storage, returnType) = declarationSpecifiersSpecific(types)
        Function(storage, returnType, functionDeclarator(declarator), compoundStatements(body))
      case declarator => // warning implicit return type 'int'
        Function(auto, int, functionDeclarator(declarator), compoundStatements(body))
    }
  }

  private val variableDeclaration: Parse[List[Declarations]] = {
    case BinaryNode("~", specifiers, expr) =>
      val (storage, declType) = declarationSpecifiersSpecific(specifiers)
      initDeclarators(expr).flatMap[Declarations, List[Declarations]] {
        case i: Identifier =>
          Declaration(storage, declType, i) :: Nil
        case f: FunctionDeclarator =>
          Declaration(storage, declType, f) :: Nil
        case a @ Assignment(i, _) =>
          Declaration(storage, declType, i) :: a :: Nil
      }
  }

  private val block: Parse[Block] = {
    case UnaryNode("B", body) => Block(compoundStatements(body))
  }

  private val multiList: Parse[List[Statements]] = {
    case BinaryNode(";", front, end) =>
      compoundStatements(front) ++ compoundStatements(end)
  }

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
    case UnaryNode("~", exprs) => LazyExpressions(expressions(exprs))
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

  private val initDeclaratorList: Parse[List[InitDeclarator]] = {
    case BinaryNode(",", front, end) =>
      initDeclarators(front) :+ initDeclarator(end)
  }

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

  private val directFunctionDeclarator: Parse[FunctionDeclarator] = {
    case UnaryNode("F", name) =>
      FunctionDeclarator(identifier(name), LAny)
    case UnaryNode("V", name) =>
      FunctionDeclarator(identifier(name), LVoid)
    case BinaryNode("F", name, args) =>
      FunctionDeclarator(identifier(name), LParam(parameters(args).toVector))
  }

  private val parameterList: Parse[List[Parameter]] = {
    case BinaryNode(",", tail, param) =>
      parameters(tail) :+ parameter(param)
  }

  private val typesAndIdentifier: Parse[Parameter] = {
    case BinaryNode("~", typeSpecifier, ident) =>
      (types(typeSpecifier), identifier(ident))
  }
}