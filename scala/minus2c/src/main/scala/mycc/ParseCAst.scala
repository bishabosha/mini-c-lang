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
import exception._
import PartialFunctionConversions._
import parseCAst._
import scala.collection.mutable

object parseCAst extends Stage {
  type Source   = CAst
  type Context  = Bindings
  type Goal     = List[Declarations]

  def apply
    ( source: Source,
      identPool: Map[String, Identifier]
    ): (Context, Goal) =
      new parseCAst(identPool, new mutable.AnyRefMap()).goal(source)
}

class parseCAst private
  ( private val identPool: Map[String, Identifier],
    private val scopedPool: mutable.AnyRefMap[(Identifier, Long),Scoped]
  ) {

  private type Parse[T] = PartialFunction[Source, T]
  private type Flatten[T] = PartialFunction[T, List[T]]
  private type FlattenO[T, O] = PartialFunction[T, O]
  private var scopeCount = 0L
  private var currentScope = scopeCount
  private var context: Context =
    Bindings.Empty
      .copy(extractDeclarations(Std.declarations)) + (ScopeKey, scopeCount)
  private var frames: List[Frame] = Nil

  private lazy val goal: Parse[(Context, Goal)] =
    translationUnit ->> { context -> _ }

  private lazy val translationUnit: Parse[Goal] =
    externalDeclarationList |
    externalDeclaration
    
  private lazy val externalDeclaration: Parse[List[Declarations]] =
    functionDefinition |
    declarationsAndAssignments !! noAssign

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

  private lazy val identifierInScope: Parse[Scoped] =
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
      identifier ->> { Cint -> })

  private lazy val compoundStatements: Parse[List[Statements]] =
    block .L |
    multiList |
    declarationsAndAssignments |
    expressionsStatement |
    jumpStatement |
    { case value => throw UnimplementedError(s"statement: $value") }

  private val externalDeclarationList: Parse[List[Declarations]] = {
    case Sequence("E", list) =>
      list.flatMap[Declarations, List[Declarations]](externalDeclaration)
  }

  private val functionDefinition: Parse[List[Declarations]] = {
    case BinaryNode("D", declarators, UnaryNode("B", body)) =>
      functionDef(declarators, Some(body))
    case BinaryNode("D", declarators, Singleton("B")) =>
      functionDef(declarators, None)
  }

  private val variableDeclaration: Parse[List[Declarations]] = {
    case BinaryNode("q", specifiers, expr) =>
      val (s, d) = declarationSpecifiersSpecific(specifiers)
      val lens = if frames.isEmpty then None else Some(Frame.localsLens)
      initDeclarators(expr).flatMap[Declarations, List[Declarations]] {
        case i: Scoped =>
          declareInScope(i, s, d, i, lens).toList
        case f @ FunctionDeclarator(id, _) =>
          declareInScope(id, s, d, f, lens).toList
        case a @ Assignment(Scoped(Identifier(i),s), StringLiteral(str)) =>
          throw SemanticError(s""""$str" can not be assigned to `$i~s`""")
        case a @ Assignment(i: Scoped, _) =>
          declareInScope(i, s, d, i, lens).toList
            .:+[Declarations, List[Declarations]](a)
        case _ =>
          Nil
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
    case Sequence(";", statements) =>
      statements.flatMap[Statements,List[Statements]](compoundStatements)
  }

  private val assignment: Parse[Assignment] = {
    case BinaryNode("=", id, value) =>
      Assignment(identifier(id), assignments(value))
  }

  private val equality: Parse[Equality] = {
    case BinaryNode("==", left, right) =>
      Equality(EQUAL, equalities(left), relationals(right))
    case BinaryNode("!=", left, right) =>
      Equality(NOT_EQUAL, equalities(left), relationals(right))
  }

  private val relational: Parse[Relational] = {
    case BinaryNode("<", left, right) =>
      Relational(LT, relationals(left), additives(right))
    case BinaryNode(">", left, right) =>
      Relational(GT, relationals(left), additives(right))
    case BinaryNode("<=", left, right) =>
      Relational(LT_EQ, relationals(left), additives(right))
    case BinaryNode(">=", left, right) =>
      Relational(GT_EQ, relationals(left), additives(right))
  }

  private val additive: Parse[Additive] = {
    case BinaryNode("+", left, right) =>
      Additive(PLUS, additives(left), multiplicatives(right))
    case BinaryNode("-", left, right) =>
      Additive(MINUS, additives(left), multiplicatives(right))
  }

  private val multiplicative: Parse[Multiplicative] = {
    case BinaryNode("*", left, right) =>
      Multiplicative(MULTIPLY, multiplicatives(left), unaries(right))
    case BinaryNode("/", left, right) =>
      Multiplicative(DIVIDE, multiplicatives(left), unaries(right))
    case BinaryNode("%", left, right) =>
      Multiplicative(MODULUS, multiplicatives(left), unaries(right))
  }

  private val unary: Parse[Unaries] = {
    case UnaryNode("+", unary) => unaries(unary)
    case UnaryNode("-", unary) => Unary(NEGATIVE, unaries(unary))
    case UnaryNode("!", unary) => Unary(NOT, unaries(unary))
  }

  private val application: Parse[Application] = {
    case BinaryNode("apply", name, args) =>
      Application(postfix(name), expressions(args))
    case UnaryNode("apply", name) =>
      Application(postfix(name), Nil)
  }

  private val identifier: Parse[Scoped] = {
    case TokenString("id", id) =>
      scopedPool.getOrElseUpdate(
        (identPool(id), currentScope),
        { Scoped(identPool(id), currentScope) }
      )
  }

  private val constant: Parse[Constant] = {
    case TokenInt("constant", id) => Constant(id)
  }

  private val lazyExpressions: Parse[LazyExpressions] = {
    case UnaryNode("~", exprs) => LazyExpressions(expressions(exprs))
  }

  private val expressionList: Parse[Expressions] = {
    case Sequence(",", assigns) =>
      assigns.map[Assignments,Expressions](assignments); 
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
    case Sequence(",", decls) =>
      decls.map[InitDeclarator,List[InitDeclarator]](initDeclarator)
  }

  private val declarationSpecifierList: Parse[List[DeclarationSpecifiers]] = {
    case Sequence("~", specifiers) =>
      specifiers.map[DeclarationSpecifiers, 
        List[DeclarationSpecifiers]](declarationSpecifier)
  }

  private val storage: Parse[Storage] = {
    case Singleton("extern") => Storage(Extern)
    case Singleton("auto") => Storage(Auto)
  }

  private val `type`: Parse[Type] = {
    case Singleton("int") => Type(Cint)
    case Singleton("function") => Type(Cfunction)
    case Singleton("void") => Type(Cvoid)
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
    case Sequence(",", params) =>
      params.map[Parameter,List[Parameter]](parameter)
  }

  private val typesAndIdentifier: Parse[Parameter] = {
    case BinaryNode("q", typeSpecifier, ident) =>
      (types(typeSpecifier), identifier(ident))
  }

  private def functionDef
    (declarators: Source, bodyOp: Option[Source]): List[Declarations] =
      declarators match {
        case BinaryNode("d", types, declarator) =>
          val (storage, typeFinal) = declarationSpecifiersSpecific(types)
          yieldDefinition(storage,typeFinal)(bodyOp) {
            functionDeclarator(declarator)
          }
        case declarator =>
          yieldDefinition(Auto, Cint)(bodyOp)(functionDeclarator(declarator))
      }

  private def yieldDefinition
    (storage: StorageTypes, types: Types)
    (bodyOp: Option[Source])
    : PartialFunction[FunctionDeclarator, List[Declarations]] = {
      case f @ FunctionDeclarator(i, args) =>
        declareInScope(i, storage, types, f, None).toList
          .:+[Declarations, List[Declarations]] {
            val bodyParsed =
              for (b <- bodyOp) yield {
                framed {
                  args match {
                    case LParam(l) => declareParamsInScope(l)
                    case _ =>
                  }
                  compoundStatements(b)
                }
              }
            if types != Cvoid then {
              tailYieldsValue(bodyParsed.map(_._2))
            }
            define {
              bodyParsed.map {
                Function(i,_,_)
              } getOrElse { 
                Function(i, Frame.Empty, Nil)
              }
            }
          }
    }

  private def framed[A](parser: => A): (Frame, A) =
    stacked {
      frames = Frame.Empty :: frames
      val result = parser
      val popped = frames.head
      frames = frames.tail
      (popped, result)
    }

  private def stacked[A](parser: => A): A = {
    context = context.push
    scopeCount += 1L
    currentScope = scopeCount
    context += (ScopeKey, currentScope)
    val result = parser
    context = context.popOrElse { Bindings.Empty }
    currentScope = getCurrentScope(context)
    result
  }

  private def declareParamsInScope(args: Vector[Parameter]): Unit = {
    for (p <- args) {
      p match {
        case (t: Types, s: Scoped) =>
          declareInScope(s, Auto, t, s, Some(Frame.paramsLens))
        case _ =>
      }
    }
  }

  import Frame._
  private def declareInScope
    ( scoped: Scoped,
      storage: StorageTypes,
      types: Types,
      declarator: Declarator,
      frameLens: Option[FrameLens]
    ): Option[Declaration] = {
      for (
        Declaration(s, t, existing) <-
          context.genGet(DeclarationKey(scoped))
      ) {
        existing match {
          case _: Scoped => declarator match {
            case _: FunctionDeclarator =>
              throw new SemanticError(
                s"Redefinition of '${scoped.id.id}' as a function type.")
            case _ =>
              throw new SemanticError(s"Redefinition of '${scoped.id.id}'.")
          }
          case _: FunctionDeclarator => declarator match {
            case f: FunctionDeclarator =>
              if existing == f && s == storage && t == types then {
                return None
              } else {
                throw new SemanticError(
                  s"Redefinition of function '${scoped.id.id}' with "+
                   "incompatible types.")
              }
            case _ =>
              throw new SemanticError(
                s"Redefinition of function '${scoped.id.id}' to variable.")
          }
        }
      }
      val declaration = Declaration(storage, types, declarator)
      context += (DeclarationKey(scoped), declaration)
      for (updater <- frameLens) {
        var declInFrame = scoped -> declaration
        frames = replaceHead(frames) { updater(_ + declInFrame) }
      }
      Some(declaration)
    }

  private def define(f: => Function): Function = {
    val definition = f
    context += (DefinitionKey(definition.id), ())
    definition
  }

  private def noAssign[A](a: A): Unit =
    a match {
      case List(_: Declaration, Assignment(Scoped(Identifier(i),_), _: Application)) =>
        throw SemanticError(
          s"Assignment of global variable $i to a function application")
      case _ =>
    }

  private def existsInScope[A](get: A => Variable): A => Unit =
    get.andThen {
      case scoped: Scoped =>
        context.genSearch(DeclarationKey(scoped)) match {
          case Some(d: Declaration) =>
            d match {
              case Declaration(_, _, Scoped(_, scope)) =>
                if currentScope != scope then {
                  if scope == 0 then {
                    frames = replaceHead(frames) {
                      Frame.globalsLens(_ + (scoped.id -> d))
                    }
                  } else {
                    if frames.head.locals.get(scoped).isEmpty then {
                      var declInFrame = scoped -> d
                      frames = replaceHead(frames) {
                        Frame.capturesLens(_ + declInFrame)
                      }
                    }
                  }
                }
              case _ =>
            }
          case None =>
            throw SemanticError(s"Identifier '${scoped.id.id}' is undefined")
        }
      case _ =>
    }

  private def tailYieldsValue(statements: Option[List[Statements]]): Unit = {
    statements match {
      case Some(body) if body.lastOption.forall(_.isInstanceOf[Return]) =>
      case _ =>
        throw SemanticError("Tail of function does not return a value.")
    }
  }

  private def reduceDeclarationSpecifiers
    (declarationSpecifiers: List[DeclarationSpecifiers])
    : (StorageTypes, Types) = {
      val (storages, types) =
          declarationSpecifiers.partition(_.isInstanceOf[Storage])
        
      val storage: StorageTypes = storages match {
        case Nil => Auto
        case (s: Storage) :: Nil => s.id
        case _ => throw SemanticError(
          "More than one storage class may not be specified.")
      }

      val returnType: Types = types match {
        case Nil => Cint // warning implicit return type 'int'
        case (t: Type) :: Nil => t.id
        case _ => throw SemanticError("Invalid combination of type specifiers.")
      }

      (storage, returnType)
    }
}