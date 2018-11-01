package mycc

import Ast._

case class Cursor(
  stack: List[Cursor],
  values: Map[Key | Temporary, Constant],
  current: Bindings
) {
  def +(pair: (Key | Temporary, Constant)): Cursor =
    Cursor(stack, values + pair, current)

  def value(key: Key | Temporary): Option[Int] =
    (this :: stack)
      .view
      .map(_.values.get(key))
      .collectFirst { case Some(o) => o.value }

  def next: Option[Cursor] =
    current.children.headOption.map { Cursor(this :: stack, Map(), _) }
}