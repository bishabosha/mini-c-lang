package mycc

import Cursor._

object Cursor {
  val Empty = Cursor(Nil, Bindings.Empty)
}

case class Cursor(stack: List[Cursor], val current: Bindings) {
  def next: Option[Cursor] =
    current.firstChild.map { copy(this :: stack, _) }
}