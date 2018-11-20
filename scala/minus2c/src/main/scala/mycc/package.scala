package object mycc {
  import Ast._
  import StorageTypes._
  import Types._
  import parseCAst._
  import exception.SemanticError
  import ArgList._
  import MIPS._

  import scala.language.implicitConversions
  implicit def bool2Int(b: Boolean): Int = if b then 1 else 0

  case class DeclarationKey(id: Identifier) extends Bindings.Key {
    type Value = Declaration
  }

  case class DefinitionKey(id: Identifier) extends Bindings.Key {
    type Value = Function
  }

  case class RegisterKey(key: Identifier | Temporary) extends Bindings.Key {
    type Value = Register
  }

  def extractDeclarations
    (declarations: List[Declaration]
    ): Map[Bindings.Key, Any] =
      (for (d @ Declaration(_, _, decl) <- declarations)
        yield (DeclarationKey(extractIdentifier(decl)), d)
      ).toMap

  private def extractIdentifier(d: Declarator): Identifier = d match {
    case i: Identifier => i
    case _ @ FunctionDeclarator(i, _) => i
  }

  def definition(id: Identifier, bindings: Bindings): Option[Function] =
    bindings.genGet(DefinitionKey(id))
  
  def defineIn
    ( key: Identifier,
      value: Function,
      bindings: Bindings
    ): Bindings =
      bindings + (DefinitionKey(key), value)

  def declareIn
    ( key: Identifier,
      value: Declaration,
      bindings: Bindings
    ): Bindings =
      bindings + (DeclarationKey(key), value)

  def scope(id: Identifier, bindings: Bindings): Option[Declaration] =
    bindings.genSearch(DeclarationKey(id))

  def local(id: Identifier, bindings: Bindings): Option[Declaration] =
    bindings.genGet(DeclarationKey(id))

  def locals(bindings: Bindings): Iterable[Declaration] =
    bindings.topView
      .collect {
        case (DeclarationKey(id), decl) => decl.asInstanceOf[Declaration]
      }

  object Std {
    val mainIdentifier = Identifier("main")
    val mainFunc =
    Declaration(
      Auto,
      Cint,
      FunctionDeclarator(mainIdentifier, LVoid)
    )
    val declarations = List[Declaration](
      Declaration(
        Extern,
        Cvoid,
        FunctionDeclarator(
          Identifier("print_int"),
          LParam(Vector(Cint -> Identifier("value")))
        )
      )
    )
  }
}