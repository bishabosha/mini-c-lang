package object mycc {
  import Ast._
  import StorageTypes._
  import Types._
  import parseCAst._
  import ArgList._
  import exception.SemanticError

  def parseAst(ast: CAst): (Context, Goal) = parseCAst(ast)

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

  def printAst(context: Context, declarations: List[Declarations]): Unit =
    printDeclarations(context, declarations, 0)
    
  private def printDeclarations(context: Context, declarations: List[Declarations], level: Int): Unit = {
    for (declaration <- declarations) {
      declaration match {
        case Declaration(storage, types, declarator) =>
          declarator match {
            case Identifier(id) => println(s"$storage $types $id;")
            case FunctionDeclarator(Identifier(id), args) => args match {
              case LVoid => printLevel(s"$storage $types $id(void);", level)
              case LAny => printLevel(s"$storage $types $id();", level)
              case LParam(params) =>
                val paramString = params.view.map {
                  case t: Types => s"$t"
                  case (t: Types, _ @ Identifier(i)) => s"$t $i"
                }.mkString("(", ", ", ")")
                printLevel(s"$storage $types $id$paramString;", level)
            }
          }
        case Assignment(Identifier(id), value) => println(s"$id = ???;")
        case Function(i @ Identifier(id), _) =>
          for (Declaration(storage, types, FunctionDeclarator(_, args)) <- context.scope(i)) {
            args match {
              case LVoid => printLevel(s"$storage $types $id(void) { ??? }", level)
              case LAny => printLevel(s"$storage $types $id() { ??? }", level)
              case LParam(params) =>
                val paramString = params.view.map {
                  case t: Types => s"$t"
                  case (t: Types, _ @ Identifier(i)) => s"$t $i"
                }.mkString("(", ", ", ")")
                printLevel(s"$storage $types $id$paramString { ??? }", level)
            }
          }
      }
    }
  }

  private def printLevel(s: String, l: Int): Unit = {
    for (_ <- 0 to l + 1) {
      print(" ")
    }
    println(s)
  }
}