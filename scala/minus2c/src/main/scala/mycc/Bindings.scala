package mycc

import Ast._
import Bindings._

object Bindings {  
  val Empty = Bindings(Map(), Nil, Nil)

  def withParents(parents: List[Bindings]) = Bindings(Map(), parents, Nil)

  def withSeen(seen: Map[Key, Declaration]) = Bindings(seen, Nil, Nil)

  def extractFrom(declarations: List[Declaration]): Map[Key, Declaration] =
    (for (d @ Declaration(_, _, decl) <- declarations)
      yield (extractIdentifier(decl), d)
    ).toMap
  
  private def extractIdentifier(d: Declarator): Identifier = d match {
    case i: Identifier => i
    case _ @ FunctionDeclarator(i, _) => i
  }
}

case class Bindings(val seen: Map[Key, Declaration],
                    val parents: List[Bindings],
                    val children: List[Bindings]) {

  def scope(id: Key): Option[Declaration] = {
      (this :: parents)
        .view
        .map(_.seen.get(id))
        .collectFirst { case Some(o) => o }
  }

  def stack: Bindings = Bindings.withParents(this :: parents)

  def popOrElse(default: => Bindings): Bindings =
    parents.headOption
           .map(_.addChild(Bindings.withSeen(seen)))
           .getOrElse(default)

  def local(id: Identifier): Option[Declaration] = seen.get(id)

  def +(pair: (Identifier, Declaration)): Bindings = Bindings(seen + pair, parents, children)

  def addChild(child: Bindings) = Bindings(seen, parents, children :+ child)
}