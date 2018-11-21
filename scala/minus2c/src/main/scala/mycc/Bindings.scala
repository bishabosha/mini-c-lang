package mycc

import Bindings._

object Bindings {
  val Empty = new Bindings(Map(), Nil, Nil)

  trait Key { type Value }

  def withParents(parents: List[Bindings]) =
    new Bindings(Map(), parents, Nil)

  def withSeen(seen: Map[Key, Any]) =
    new Bindings(seen, Nil, Nil)
}

class Bindings private
  (
    private val seen: Map[Key, Any],
    parents: List[Bindings],
    children: List[Bindings]
  ) {
    def genGet(id: Key): Option[id.Value] =
      seen.get(id).map(_.asInstanceOf[id.Value])

    def stack: Iterable[Bindings] = (this :: parents).view

    def genSearch(id: Key): Option[id.Value] =
      stack
        .map(_.seen.get(id))
        .collectFirst { case Some(o) => o.asInstanceOf[id.Value] }

    def topView: Iterable[(Key, Any)] = seen.view

    def stacked: Bindings = Bindings.withParents(this :: parents)

    def popOrElse(default: => Bindings): Bindings =
      parents.headOption
             .map(_.addChild(Bindings.withSeen(seen)))
             .getOrElse(default)

    def +(key: Key, value: key.Value): Bindings =
      new Bindings(seen + (key -> value), parents, children)

    def -(key: Key): Bindings =
      new Bindings(seen - key, parents, children)

    def addChild(child: Bindings) =
      new Bindings(seen, parents, children :+ child)

    def firstChild: Option[Bindings] = children.headOption
  }