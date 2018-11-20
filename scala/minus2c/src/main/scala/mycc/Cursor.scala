package mycc

import Ast._

object Cursor {
  trait Key { type Value }
}

import Cursor._

case class Cursor(
  stack: List[Cursor],
  values: Map[Cursor.Key, Any],
  current: Bindings
) {
  def +(key: Cursor.Key, value: key.Value): Cursor =
    Cursor(stack, values + (key -> value), current)

  def value(key: Cursor.Key): Option[key.Value] =
    (this :: stack)
      .view
      .map(_.values.get(key))
      .collectFirst { case Some(o) => o.asInstanceOf[key.Value] }

  def next: Option[Cursor] =
    current.children.headOption.map { Cursor(this :: stack, Map(), _) }
}