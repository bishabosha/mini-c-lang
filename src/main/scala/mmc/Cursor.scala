package mmc

import Cursor._
import exception._

object Cursor
  def apply(bindings: Bindings): Cursor =
    new Cursor(bindings, bindings.removeChildren :: Nil)

class Cursor private (val current: Bindings, queue: List[Bindings])
  def next: Cursor =
    current.children.foldRight(queue)((child, acc) => child.updateParents(current) :: current.removeChildren :: acc)
    match
      case head :: tail => new Cursor(head, tail)
      case _            => throw SemanticError("Unexpected next call on Cursor")

  def isEmpty: Boolean =
    current.children.isEmpty && queue.isEmpty

  def withBindings(bindings: Bindings): Cursor =
    new Cursor(bindings, queue)
