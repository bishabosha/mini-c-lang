package mycc

import Ast._

object Bindings {
  val Empty = Bindings(Map(), Nil, Nil)
}

case class Bindings(val seen: Map[Identifier, Declaration],
                    val parents: List[Bindings],
                    val children: List[Bindings]) {

  def scope(id: Identifier): Option[Declaration] = {
      (this :: parents)
        .view
        .map(_.seen.get(id))
        .collectFirst { case Some(o) => o }
  }

  def stack: Bindings = Bindings.Empty.withParents(this :: parents)

  def popOrElse(default: => Bindings): Bindings = parents match {
    case head :: tail => head.addChild(this.withParents(Nil))
    case Nil => default
  }

  def local(id: Identifier): Option[Declaration] = seen.get(id)

  def +(pair: (Identifier, Declaration)): Bindings = Bindings(seen + pair, parents, children)

  def addChild(child: Bindings) = Bindings(seen, parents, children :+ child)

  def withParents(updated: List[Bindings]) = Bindings(seen, updated, children)
}