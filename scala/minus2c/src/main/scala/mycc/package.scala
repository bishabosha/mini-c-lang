package object mycc {
  import Ast._
  import StorageTypes._
  import Types._
  import exception.SemanticError

  def parseAst(ast: CAst): List[Ast] = ParseCAst(ast)

  def reduceDeclarationSpecifiers(declarationSpecifiers: List[DeclarationSpecifiers]): (StorageTypes, Types) = {
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