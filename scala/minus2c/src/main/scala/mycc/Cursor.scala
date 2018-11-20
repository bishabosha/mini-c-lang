package mycc

import Cursor._

object Cursor {
  val Empty = new Cursor(Nil, Bindings.Empty)
}

class Cursor private (stack: List[Cursor], val current: Bindings) {
  def +(key: Bindings.Key, value: key.Value): Cursor =
    new Cursor(stack, current + (key, value))

  def value(key: Bindings.Key): Option[key.Value] =
    current.genSearch(key)

  def next: Option[Cursor] =
    current.firstChild.map { new Cursor(this :: stack, _) }

  def withBindings(bindings: Bindings) = new Cursor(stack, bindings)
}