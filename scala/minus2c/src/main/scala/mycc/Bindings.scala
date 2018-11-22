package mycc

import Bindings._

object Bindings {
  val Empty = Bindings(Map(), Nil, Nil)
  trait Key { type Value }
}

case class Bindings
  ( data: Map[Key, Any],
    stack: List[Bindings],
    children: List[Bindings]
  ) {
    def genGet(id: Key): Option[id.Value] =
      data.get(id).map(_.asInstanceOf[id.Value])

    def genSearch(id: Key): Option[id.Value] =
      (Stream(this) ++: stack.view)
        .map(_.data.get(id))
        .collectFirst { case Some(o) => o.asInstanceOf[id.Value] }

    def topView: Iterable[(Key, Any)] = data.view

    def stacked: Bindings = Empty.copy(stack = this :: stack)

    def popOrElse(default: => Bindings): Bindings =
      stack.headOption
           .map(_.addChild(Empty.copy(data)))
           .getOrElse(default)

    def updateParents(parent: Bindings): Bindings =
      copy(stack = parent :: parent.stack)

    def +(key: Key, value: key.Value): Bindings =
      copy(data + (key -> value))

    def -(key: Key): Bindings =
      copy(data - key)

    def addChild(child: Bindings) =
      copy(children = children :+ child)

    def firstChild: Option[Bindings] = children.headOption

    override def toString = s"Bindings(seen = $data)"
  }