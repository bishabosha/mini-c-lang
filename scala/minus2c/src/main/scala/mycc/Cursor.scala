package mycc

import Ast._

case class Cursor[Values](
  stack: List[Cursor[Values]],
  values: Map[Key | Temporary, Values],
  current: Bindings
) {
  def +(pair: (Key | Temporary, Values)): Cursor[Values] =
    Cursor(stack, values + pair, current)

  def value(key: Key | Temporary): Option[Values] =
    (this :: stack)
      .view
      .map(_.values.get(key))
      .collectFirst { case Some(o) => o }

  def next: Option[Cursor[Values]] =
    current.children.headOption.map { Cursor(this :: stack, Map(), _) }
}