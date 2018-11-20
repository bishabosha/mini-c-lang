package object mycc {
  import Ast._
  import StorageTypes._
  import Types._
  import parseCAst._
  import exception.SemanticError
  import ArgList._

  import scala.language.implicitConversions
  implicit def bool2Int(b: Boolean): Int = if b then 1 else 0

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