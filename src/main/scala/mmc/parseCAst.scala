package mmc

import mmclib._
import DSL._
import Ast._
import Constants._
import Type._
import Frame._
import ArgList._
import StorageKind._
import EqualityOperators._
import RelationalOperators._
import AdditiveOperators._
import MultiplicativeOperators._
import UnaryOperators._
import exception._
import PartialFunctionConversions._
import parseCAst._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonLocalReturns._

object parseCAst extends Stage:
  type Source  = CAst
  type Context = Bindings
  type Goal    = List[Declarations]

  def apply(source: Source, identPool: Set[Identifier]): (Context, Goal) =
    new parseCAst(identPool, mutable.AnyRefMap.empty).goal(source)

class parseCAst private (
  private val identPool: Set[Identifier],
  private val scopedPool: mutable.AnyRefMap[(Identifier, Long),Scoped]
):
  private type Parse[T] = PartialFunction[Source, T]
  private type Flatten[T] = PartialFunction[T, List[T]]
  private type FlattenO[T, O] = PartialFunction[T, O]
  private var inDecl = false
  private var ifCount = 0L
  private var scopeCount = 0L
  private var currentScope = scopeCount
  private var context = Bindings.empty.copy(extractDeclarations(Std.declarations: _*).toMap).add(ScopeKey, scopeCount)

  private var frames: List[Frame] = Nil

  private lazy val goal: Parse[(Context, Goal)]
    = translationUnit ->> (context -> _)

  private lazy val translationUnit: Parse[Goal]
    = externalDeclarationList
    | externalDeclaration

  private lazy val externalDeclaration: Parse[List[Declarations]]
    = functionDefinition
    | declarationsAndAssignments !! noAssign

  private lazy val declarationsAndAssignments: Parse[List[Declarations]]
    = variableDeclaration
    | functionDefinition
    | declarationSpecifier .E

  private lazy val declarationSpecifiers: Parse[List[DeclarationSpecifiers]]
    = declarationSpecifierList
    | declarationSpecifier .L

  private lazy val declarationSpecifier: Parse[DeclarationSpecifiers]
    = `type`
    | storage

  private lazy val declarationSpecifiersSpecific: Parse[(StorageKind, Type)]
    = declarationSpecifiers ->> reduceDeclarationSpecifiers

  private lazy val jumpStatement: Parse[List[Statements]]
    = `return` .L

  private lazy val expressionsStatement: Parse[Expressions]
    = empty
    | expressions

  private lazy val expressions: Parse[Expressions]
    = expressionList
    | assignmentsAsExpressions

  private lazy val assignmentsAsExpressions: Parse[Expressions]
    = assignments .L

  private lazy val assignments: Parse[Assignments]
    = assignment
    | equalities

  private lazy val equalities: Parse[Equalities]
    = equality
    | relationals

  private lazy val relationals: Parse[Relationals]
    = relational
    | additives

  private lazy val additives: Parse[Additives]
    = additive
    | multiplicatives

  private lazy val multiplicatives: Parse[Multiplicatives]
    = multiplicative
    | unaries

  private lazy val unaries: Parse[Unaries]
    = unary
    | postfix

  private lazy val postfix: Parse[Postfix]
    = application
    | primary

  private lazy val primary: Parse[Primary]
    = identifierSearchScope
    | (intLiteral ->> (Constant(_))
      | (lazyExpressions
        | stringLiteral ->> (Constant(_))))

  private lazy val identifierSearchScope: Parse[Scoped]
    = identifier ->> searchInScope

  private def identifierWithScope: Parse[Scoped]
    = identifierWithScopeOf(currentScope)

  private def identifierWithScopeOf(scope: Long): Parse[Scoped]
    = identifier ->> (decld(scoped(_, scope)))

  private lazy val initDeclarators: Parse[List[InitDeclarator]]
    = initDeclaratorList
    | initDeclarator .L

  private lazy val initDeclarator: Parse[InitDeclarator]
    = declarator
    | declassignment

  private lazy val declarator: Parse[InitDeclarator]
    = identifierWithScope
    | functionDeclarator

  private lazy val types: Parse[Type]
    = `type` ->> (_.id)

  private lazy val parameters: Parse[List[Parameter]]
    = parameterList
    | parameter .L

  private lazy val parameter: Parse[Parameter]
    = typesAndIdentifier
    | (types
      | identifierWithScopeOf(currentScope + 1) ->> (Cint -> _))

  private lazy val compoundStatements: Parse[List[Statements]]
    = block ->> (_.toList)
    | multiList
    | declarationsAndAssignments
    | expressionsStatement
    | jumpStatement
    | selections .L
    | { case value => throw UnimplementedError(s"statement (${value.ast.tpe}):\n${value.show}") }

  private lazy val selections: Parse[IfElse] = ifElse

  private val ifElse: Parse[Selections] =
    case BinaryNode("if", test, BinaryNode("else", ifThen, orElse)) =>
      makeIfElse(test, inlineBlock(ifThen), inlineBlock(orElse))
    case BinaryNode("if", test, ifThen) =>
      makeIfElse(test, inlineBlock(ifThen), None)

  @tailrec
  private def inlineBlock(node: Source): Option[Source] = node match
    case Singleton("B")        => None
    case UnaryNode("B", stats) => inlineBlock(stats)
    case other                 => Some(other)

  private val externalDeclarationList: Parse[List[Declarations]] =
    case Sequence("E", list) =>
      list.flatMap(externalDeclaration)

  private val functionDefinition: Parse[List[Declarations]] =
    case BinaryNode("D", declarators, UnaryNode("B", body)) => functionDef(declarators, Some(body))
    case BinaryNode("D", declarators, Singleton("B"))       => functionDef(declarators, None)

  private val variableDeclaration: Parse[List[Declarations]] =
    case BinaryNode("q", specifiers, expr) =>
      val s -> d = declarationSpecifiersSpecific(specifiers)
      val lens   = if frames.isEmpty then None else Some(Frame.localsLens)
      initDeclarators(expr).flatMap:
        case i: Scoped =>
          declareInScope(i, s, d, i, lens).toList
        case f @ FunctionDeclarator(id, _) =>
          declareInScope(id, s, d, f, lens).toList
        case a @ Assignment(Scoped(i,s), Constant(str: StringLiteral)) =>
          throw SemanticError(s""""$str" can not be assigned to `$i~s`""")
        case a @ Assignment(i: Scoped, _) =>
          declareInScope(i, s, d, i, lens).toList :+ a
        case _ =>
          Nil

  private val block: Parse[Option[Block]] =
    case UnaryNode("B", body) => inlineBlock(body).map(stats => Block(stacked(compoundStatements(stats))))
    case Singleton("B")       => None

  private val multiList: Parse[List[Statements]] =
    case Sequence(";", statements) => statements.flatMap(compoundStatements)

  private val declassignment: Parse[Assignment] =
    case BinaryNode("=", id, value) => Assignment(identifierWithScope(id), assignments(value))

  private val assignment: Parse[Assignment] =
    case BinaryNode("=", id, value) => Assignment(identifierSearchScope(id), assignments(value))

  private val equality: Parse[Equality] =
    case BinaryNode("==", left, right) => Equality(EQUAL, equalities(left), relationals(right))
    case BinaryNode("!=", left, right) => Equality(NOT_EQUAL, equalities(left), relationals(right))

  private val relational: Parse[Relational] =
    case BinaryNode("<", left, right)  => Relational(LT, relationals(left), additives(right))
    case BinaryNode(">", left, right)  => Relational(GT, relationals(left), additives(right))
    case BinaryNode("<=", left, right) => Relational(LT_EQ, relationals(left), additives(right))
    case BinaryNode(">=", left, right) => Relational(GT_EQ, relationals(left), additives(right))

  private val additive: Parse[Additive] =
    case BinaryNode("+", left, right) => Additive(PLUS, additives(left), multiplicatives(right))
    case BinaryNode("-", left, right) => Additive(MINUS, additives(left), multiplicatives(right))

  private val multiplicative: Parse[Multiplicative] =
    case BinaryNode("*", left, right) => Multiplicative(MULTIPLY, multiplicatives(left), unaries(right))
    case BinaryNode("/", left, right) => Multiplicative(DIVIDE, multiplicatives(left), unaries(right))
    case BinaryNode("%", left, right) => Multiplicative(MODULUS, multiplicatives(left), unaries(right))

  private val unary: Parse[Unaries] =
    case UnaryNode("+", unary) => unaries(unary)
    case UnaryNode("-", unary) => Unary(NEGATIVE, unaries(unary))
    case UnaryNode("!", unary) => Unary(NOT, unaries(unary))

  private val application: Parse[Application] =
    case BinaryNode("apply", name, args) => Application(postfix(name), expressions(args))
    case UnaryNode("apply", name)        => Application(postfix(name), Nil)

  private val identifier: Parse[Identifier] =
    case TokenString("id", id) => id.ensuring(identPool(id), s"unknown identifier $id")

  private val intLiteral: Parse[IntLiteral] =
    case TokenInt("constant", id) => IntLiteral(id)

  private val lazyExpressions: Parse[LazyExpressions] =
    case UnaryNode("~", exprs) => LazyExpressions(expressions(exprs))

  private val expressionList: Parse[Expressions] =
    case Sequence(",", assigns) => assigns.map(assignments)

  private val stringLiteral: Parse[StringLiteral] =
    case TokenString("string", value) => StringLiteral(value)

  private val `return`: Parse[Return] =
    case UnaryNode("return", value) => Return(expressions(value))
    case Singleton("return")        => Return(Nil)

  private val empty: Parse[Expressions] =
    case Singleton("ø") => Nil

  private val initDeclaratorList: Parse[List[InitDeclarator]] =
    case Sequence(",", decls) => decls.map(initDeclarator)

  private val declarationSpecifierList: Parse[List[DeclarationSpecifiers]] =
    case Sequence("~", specifiers) => specifiers.map(declarationSpecifier)

  private val storage: Parse[Storage] =
    case Singleton("extern") => Storage(Extern)
    case Singleton("auto")   => Storage(Auto)

  private val `type`: Parse[TypeRef] =
    case Singleton("int")      => TypeRef(Cint)
    case Singleton("function") => TypeRef(Cfunction)
    case Singleton("void")     => TypeRef(Cvoid)

  private val functionDeclarator: Parse[FunctionDeclarator] =
    case UnaryNode("F", name)        => FunctionDeclarator(identifierWithScope(name), LAny)
    case UnaryNode("V", name)        => FunctionDeclarator(identifierWithScope(name), LVoid)
    case BinaryNode("F", name, args) => FunctionDeclarator(identifierWithScope(name), LParam(parameters(args)))

  private val parameterList: Parse[List[Parameter]] =
    case Sequence(",", params) => params.map(parameter)

  private val typesAndIdentifier: Parse[Parameter] =
    case BinaryNode("q", tpe, ident) => types(tpe) -> identifierWithScopeOf(currentScope + 1)(ident)

  private def makeIfElse(test: Source, ifThen: Option[Source], orElse: Option[Source]): IfElse =
    val id = ifCount
    ifCount += 1
    IfElse(
      id,
      expressions(test),
      ifThen.fold(Nil)(s => stacked(compoundStatements(s))),
      orElse.fold(Nil)(s => stacked(compoundStatements(s)))
    )

  private def functionDef(declarators: Source, bodyOp: Option[Source]): List[Declarations] = declarators match
    case BinaryNode("d", types, declarator) =>
      val (storage, typeFinal) = declarationSpecifiersSpecific(types)
      yieldDefinition(storage,typeFinal)(bodyOp)(functionDeclarator(declarator))

    case declarator => yieldDefinition(Auto, Cint)(bodyOp)(functionDeclarator(declarator))

  private def yieldDefinition(storage: StorageKind, types: Type)(body: Option[Source]): FunctionDeclarator => List[Declarations] =
    case f @ FunctionDeclarator(i, args) =>
      declareInScope(i, storage, types, f, None).toList :+ makeDef(i, types, args, body)

  private def makeDef(i: Scoped, types: Type, args: ArgList, body: Option[Source]): Declarations =
    val bodyParsed =
    for b <- body
    yield framed:
      args match
        case LParam(ls) => declareParamsInScope(ls:_*)
        case _          =>
      compoundStatements(b)
    if types != Cvoid then
      tailYieldsValue(bodyParsed.map(_._2))
    define(bodyParsed.map(Function(i,_,_)).getOrElse(Function(i, Frame.empty, Nil)))

  private def scoped(id: Identifier, scope: Long): Scoped =
    scopedPool.getOrElseUpdate(id -> scope, Scoped(id, scope))

  private def framed[A](parser: => A): (Frame, A) =
    stacked:
      frames = Frame.empty :: frames
      val result = parser
      val popped = frames.head
      frames = frames.tail
      popped -> result

  private def stacked[A](parser: => A): A =
    context = context.push
    scopeCount += 1L
    currentScope = scopeCount
    context = context.add(ScopeKey, currentScope)
    val result = parser
    context = context.popOrElse(Bindings.empty)
    currentScope = getCurrentScope(context)
    result

  private def decld[A](parser: => A): A =
    if inDecl then throw SemanticError("already in decl!")
    inDecl = true
    val result = parser
    if !inDecl then throw SemanticError("no longer in decl!")
    inDecl = false
    result

  private def declareParamsInScope(args: Parameter*): Unit =
    for (t -> s) <- args do
      declareInScope(s, Auto, t, s, Some(Frame.paramsLens))

  private def declareInScope(scoped: Scoped, storage: StorageKind, types: Type, declarator: Declarator,
    frameLens: Option[FrameLens])
  : Option[Declaration] =
    returning:
      for
        Declaration(s, t, existing) <- context.genGet(DeclarationKey(scoped.id))
      do
        existing match
          case _: Scoped => declarator match
            case _: FunctionDeclarator =>
              throw new SemanticError(
                s"Redefinition of '${scoped.id}' as a function type.")
            case _ =>
              throw new SemanticError(s"Redefinition of '${scoped.id}'.")
          case _: FunctionDeclarator => declarator match
            case f: FunctionDeclarator =>
              if existing == f && s == storage && t == types then
                throwReturn(Option.empty)
              else
                throw new SemanticError(
                  s"Redefinition of function '${scoped.id}' with "+
                  "incompatible types.")
            case _ =>
              throw new SemanticError(
                s"Redefinition of function '${scoped.id}' to variable.")
      val declaration = Declaration(storage, types, declarator)
      context = context.add(DeclarationKey(scoped.id), declaration)
      for updater <- frameLens do
        var declInFrame = scoped -> declaration
        frames = replaceHead(frames)(updater(_ + declInFrame))
      Some(declaration)

  private def define(f: Function): Function =
    context = context.add(DefinitionKey(f.id), ())
    f

  private def noAssign[A](a: A): Unit =
    a match
      case List(_: Declaration, Assignment(Scoped(i,_), _: Application)) =>
        throw SemanticError(
          s"Assignment of global variable $i to a function application")
      case _ =>

  private def searchInScope(identifier: Identifier): Scoped =
    context.genSearch(DeclarationKey(identifier)) match
      case Some(d @ Declaration(_, _, decl)) =>
        decl match
          case Scoped(_, scope) =>
            val scopedId = scoped(identifier, scope)
            if currentScope != scope then
              if scope == 0 then
                frames = replaceHead(frames)(Frame.globalsLens(_ + (identifier -> d)))
              else if frames.head.locals.get(scopedId).isEmpty then
                var declInFrame = scopedId -> d
                frames = replaceHead(frames)(Frame.capturesLens(_ + declInFrame))
            scopedId

          case FunctionDeclarator(Scoped(_, scope), _) => scoped(identifier, scope)

      case _ =>
        if inDecl then
          scoped(identifier, currentScope)
        else
          throw SemanticError(s"Identifier '$identifier~$currentScope' is undefined")

  private def tailYieldsValue(statements: Option[List[Statements]]): Unit =
    statements match
      case Some(body) if body.lastOption.forall(_.isInstanceOf[Return]) =>
      case _ =>
        throw SemanticError("Tail of function does not return a value.")

  private def reduceDeclarationSpecifiers(declarationSpecifiers: List[DeclarationSpecifiers]): (StorageKind, Type) =
    val storages -> types = declarationSpecifiers.partition(_.isInstanceOf[Storage])

    val storage = storages match
      case Nil                 => Auto
      case (s: Storage) :: Nil => s.id
      case _                   => throw SemanticError("More than one storage class may not be specified.")

    val returnType = types match
      case Nil                 => Cint // warning implicit return type 'int'
      case (t: TypeRef) :: Nil => t.id
      case _                   => throw SemanticError("Invalid combination of type specifiers.")

    storage -> returnType
