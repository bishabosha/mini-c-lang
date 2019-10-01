package mmc

import Bindings._

object Bindings

  val empty = Bindings(Map.empty, Nil, Nil)

  trait Key
    type Value

case class Bindings(
  data: Map[Key, Key#Value],
  stack: List[Bindings],
  children: List[Bindings]
)
  def genGet(id: Key): Option[id.Value] =
    data.get(id).map(_.asInstanceOf[id.Value])

  def genSearch(id: Key): Option[id.Value] =
    (LazyList(this) ++: stack.view)
      .map(_.data.get(id))
      .collectFirst({ case Some(o) => o.asInstanceOf[id.Value] })

  def topView: Iterable[(Key, Any)] = data.view

  def push: Bindings = empty.copy(stack = this :: stack)

  def popOrElse(default: => Bindings): Bindings =
    stack.headOption
          .map(_.addChild(this))
          .getOrElse(default)

  def updateParents(parent: Bindings): Bindings =
    copy(stack = parent :: parent.stack)

  def add(key: Key, value: key.Value): Bindings =
    copy(data + (key -> value))

  def -(key: Key): Bindings =
    copy(data - key)

  def addChild(child: Bindings) =
    copy(children = children :+ child)

  def firstChild: Option[Bindings] = children.headOption

  def removeChildren: Bindings = copy(data,stack,Nil)

  override def toString = s"Bindings(seen = $data)"
