package mycc

import Ast._

case class Bindings(val seen: Map[String, Declarator],
                    val parents: List[Bindings]) {

  def canAssign(id: String): Option[Declarator] = {
    def lookup(id: String): Iterable[Option[Declarator]] = for {
      b <- (this :: parents).view
    } yield b.seen.get(id)
    lookup(id).collectFirst { case Some(o) => o }
  }

  def canDeclare(id: String): Option[Declarator] = seen.get(id)

  def +(pair: (String, Declarator)): Bindings = Bindings(seen + pair, parents)
}