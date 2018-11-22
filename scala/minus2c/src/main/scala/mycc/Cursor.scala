package mycc

import Cursor._
import mycc.exception.SemanticError

object Cursor {
  def apply(bindings: Bindings): Cursor =
    new Cursor(bindings, Nil)
}

class Cursor private (val current: Bindings, queue: List[Bindings]) {
  def next: Cursor = {
    current.children.foldRight(queue) { (child, acc) =>
      child.updateParents(current) :: acc
    } match {
      case head :: tail =>
        new Cursor(head, tail)
      case _ =>
        throw SemanticError("Unexpected next call on Cursor")
    }
  }

  def withBindings(bindings: Bindings): Cursor =
    new Cursor(bindings, queue)
}