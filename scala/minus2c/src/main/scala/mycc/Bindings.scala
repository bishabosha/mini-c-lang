package mycc

import Ast._

object Bindings {
  def default = Bindings(Map(), Nil)
}

case class Bindings(val seen: Map[Identifier, Declaration],
                    val parents: List[Bindings]) {

  def scope(id: Identifier): Option[Declaration] = {
    def lookup(id: Identifier): Iterable[Option[Declaration]] = for {
      b <- (this :: parents).view
    } yield b.seen.get(id)
    lookup(id).collectFirst { case Some(o) => o }
  }

  def local(id: Identifier): Option[Declaration] = seen.get(id)

  def +(pair: (Identifier, Declaration)): Bindings = Bindings(seen + pair, parents)
}