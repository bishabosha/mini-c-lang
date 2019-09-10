package mmc

type Variable   = Scoped | Temporary
type Identifier = String

class Temporary
case class Scoped(id: Identifier, scope: Long)

object Constants {
  opaque type StringLiteral = String
  opaque type IntLiteral    = Int

  def IntLiteral(i: Int): IntLiteral = i
  def StringLiteral(s: String): StringLiteral = s

  object IntLiteral {
    given {
      def (c: IntLiteral) value: Int = c
    }
  }

  object StringLiteral {
    given {
      def (s: StringLiteral) value: String = s
    }
  }
}
