package object mycc {
  import Ast._
  import StorageTypes._
  import Types._
  import parseCAst._
  import exception.SemanticError

  import scala.language.implicitConversions
  implicit def bool2Int(b: Boolean): Int = if (b) 1 else 0
}