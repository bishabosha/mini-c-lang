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
import parseCAst._
import scala.collection.mutable

object Std {
  val declarations = List[Declaration](
    Declaration(extern, void, FunctionDeclarator(Identifier("print_int"), LParam(Vector((int, Identifier("value"))))))
  )
}

object parseCAst extends Stage {
  type Source   = CAst
  type Context  = Bindings
  type Goal     = List[Declarations]

  override def apply(source: Source): (Context, Goal) = new parseCAst().goal(source)
}

class parseCAst private {

  private type Parse[T] = PartialFunction[Source, T]
  private type Flatten[T] = PartialFunction[T, List[T]]
  private type FlattenO[T, O] = PartialFunction[T, O]
  private var context: Context = Bindings.withSeen(Bindings.extractFrom(Std.declarations))
  private val identPool = new mutable.AnyRefMap[String, Identifier]()

  private lazy val goal: Parse[(Context, Goal)] =
    translationUnit ->> { goal => context -> goal }

  private lazy val translationUnit: Parse[Goal] =
    externalDeclarationList |
    externalDeclaration
    
  private lazy val externalDeclaration: Parse[List[Declarations]] =
    functionDefinition |
    declarationsAndAssignments

  private lazy val declarationsAndAssignments: Parse[List[Declarations]] =
    variableDeclaration |
    functionDefinition |
    declarationSpecifier .E

  private lazy val declarationSpecifiers: Parse[List[DeclarationSpecifiers]] =
    declarationSpecifierList |
    declarationSpecifier .L

  private lazy val declarationSpecifier: Parse[DeclarationSpecifiers] =
    `type` |
    storage

  private lazy val declarationSpecifiersSpecific: Parse[(StorageTypes, Types)] =
    declarationSpecifiers ->> reduceDeclarationSpecifiers

  private lazy val jumpStatement: Parse[List[Statements]] =
    `return` .L

  private lazy val expressionsStatement: Parse[Expressions] =
    empty |
    expressions

  private lazy val expressions: Parse[Expressions] =
    expressionList |
    assignmentsAsExpressions

  private lazy val assignmentsAsExpressions: Parse[Expressions] =
    assignments .L

  private lazy val assignments: Parse[Assignments] =
    assignment !! existsInScope(_.lvalue) |
    equalities

  private lazy val equalities: Parse[Equalities] =
    equality |
    relationals

  private lazy val relationals: Parse[Relationals] =
    relational |
    additives

  private lazy val additives: Parse[Additives] =
    additive |
    multiplicatives

  private lazy val multiplicatives: Parse[Multiplicatives] =
    multiplicative |
    unaries

  private lazy val unaries: Parse[Unaries] =
    unary |
    postfix

  private lazy val postfix: Parse[Postfix] =
    application |
    primary

  private lazy val primary: Parse[Primary] =
    identifierInScope |
    (constant |
      (lazyExpressions |
        stringLiteral))

  private lazy val identifierInScope: Parse[Identifier] = 
    identifier !! existsInScope(identity)

  private lazy val initDeclarators: Parse[List[InitDeclarator]] =
    initDeclaratorList |
    initDeclarator .L

  private lazy val initDeclarator: Parse[InitDeclarator] =
    declarator |
    assignment 

  private lazy val declarator: Parse[InitDeclarator] =
    identifier |
    functionDeclarator

  private lazy val types: Parse[Types] =
    `type` ->> { _.id }

  private lazy val parameters: Parse[List[Parameter]] =
    parameterList |
    parameter .L

  private lazy val parameter: Parse[Parameter] =
    typesAndIdentifier |
    (types |
      identifier ->> { int -> })

  private lazy val compoundStatements: Parse[List[Statements]] =
    block .L |
    multiList |
    declarationsAndAssignments |
    expressionsStatement |
    jumpStatement |
    { case value => throw UnimplementedError(s"statement: $value") }

  private val externalDeclarationList: Parse[List[Declarations]] = {
    case BinaryNode("E", list, decl) =>
      translationUnit(list)
        .++[Declarations, List[Declarations]](externalDeclaration(decl))
  }

  private val functionDefinition: Parse[List[Declarations]] = {
    case BinaryNode("D", declarators, UnaryNode("B", body)) =>
      functionDef(declarators, Some(body))
    case BinaryNode("D", declarators, Singleton("B")) =>
      functionDef(declarators, None)
  }

  private val variableDeclaration: Parse[List[Declarations]] = {
    case BinaryNode("~", specifiers, expr) =>
      val (s, d) = declarationSpecifiersSpecific(specifiers)
      initDeclarators(expr).flatMap[Declarations, List[Declarations]] {
        case i: Identifier =>
          declareInScope(i, s, d, i).toList
        case f @ FunctionDeclarator(id, _) =>
          declareInScope(id, s, d, f).toList
        case a @ Assignment(i, _) =>
          declareInScope(i, s, d, i).toList
            .:+[Declarations, List[Declarations]](a)
      }
  }

  private val block: Parse[Block] = {
    case UnaryNode("B", body) =>
      stacked {
        Block(compoundStatements(body))
      }
    case Singleton("B") =>
      Block(Nil)
  }

  private val multiList: Parse[List[Statements]] = {
    case BinaryNode(";", front, end) =>
      compoundStatements(front) ++ compoundStatements(end)
  }

  private val assignment: Parse[Assignment] = {
    case BinaryNode("=", id, value) => Assignment(identifier(id), assignments(value))
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
    case UnaryNode("+", unary) => Unary(POSITIVE, unaries(unary))
    case UnaryNode("-", unary) => Unary(NEGATIVE, unaries(unary))
    case UnaryNode("!", unary) => Unary(NOT, unaries(unary))
  }

  private val application: Parse[Application] = {
    case BinaryNode("apply", name, args) => Application(postfix(name), expressions(args))
    case UnaryNode("apply", name) => Application(postfix(name), Nil)
  }

  private val identifier: Parse[Identifier] = {
    case TokenString("id", id) => identPool.getOrElseUpdate(id, Identifier(id))
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

  private val functionDeclarator: Parse[FunctionDeclarator] = {
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

  private def functionDef(declarators: Source, bodyOp: Option[Source]): List[Declarations] = declarators match {
    case BinaryNode("d", types, declarator) =>
      yieldDefinition(declarationSpecifiersSpecific(types), bodyOp) { functionDeclarator(declarator) }
    case declarator =>
      yieldDefinition((auto, int), bodyOp) { functionDeclarator(declarator) }
  }

  private def yieldDefinition(
    declarators: (StorageTypes, Types),
    bodyOp: Option[Source]
  ): PartialFunction[FunctionDeclarator, List[Declarations]] = {
    case f @ FunctionDeclarator(i, args) =>
      declareInScope(i, declarators._1, declarators._2, f).toList
        .:+[Declarations, List[Declarations]] {
          val bodyParsed =
            for (b <- bodyOp) yield {
              stacked {
                args match {
                  case LParam(l) => declareParamsInScope(l)
                  case _ =>
                }
                compoundStatements(b)
              }
            }
          if (declarators._2 != void) tailYieldsValue(bodyParsed)
          define {
            Function(i, bodyParsed.getOrElse { Nil })
          }
        }
  }    

  private def stacked[A](parser: => A): A = {
    context = context.stack
    val result = parser
    context = context.popOrElse { Bindings.Empty }
    result
  }

  private def declareParamsInScope(args: Vector[Parameter]): Unit = {
    for (p <- args) {
      p match {
        case (t: Types, i: Identifier) =>
          declareInScope(i, auto, t, i)
        case _ =>
      }
    }
  }

  private def declareInScope(
    identifier: Identifier,
    storage: StorageTypes,
    types: Types,
    declarator: Declarator
  ): Option[Declaration] = {
    for (Declaration(s, t, existing) <- context.local(identifier)) existing match {
      case _: Identifier => declarator match {
        case _: FunctionDeclarator =>
          throw new SemanticError(s"Redefinition of '${identifier.id}' as a function type.")
        case _ =>
          throw new SemanticError(s"Redefinition of '${identifier.id}'.")
      }
      case _: FunctionDeclarator => declarator match {
        case f: FunctionDeclarator =>
          if (existing == f && s == storage && t == types) {
            return None
          } else {
            throw new SemanticError(s"Redefinition of function '${identifier.id}' with incompatible types.")
          }
        case _ =>
          throw new SemanticError(s"Redefinition of function '${identifier.id}' to variable.")
      }
    }
    val declaration = Declaration(storage, types, declarator)
    context = context + (identifier -> declaration)
    Some(declaration)
  }

  private def define(f: => Function): Function = {
    val definition = f
    context = context.define(definition.id -> definition)
    definition
  }

  private def existsInScope[A](get: A => Identifier): A => Unit =
    get.andThen { ident =>
      if (!context.scope(ident).isDefined) {
        throw SemanticError(s"Identifier '${ident.id}' is undefined")
      }
    }

  private def tailYieldsValue(statements: Option[List[Statements]]): Unit =
    statements match {
      case Some(body) if body.lastOption.forall(_.isInstanceOf[Return]) =>
      case _ =>
        throw SemanticError("Tail of function does not return a value.")
    }

  private def reduceDeclarationSpecifiers(declarationSpecifiers: List[DeclarationSpecifiers]): (StorageTypes, Types) = {
    val (storages, types) =
        declarationSpecifiers.partition(_.isInstanceOf[Storage])
        
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
}