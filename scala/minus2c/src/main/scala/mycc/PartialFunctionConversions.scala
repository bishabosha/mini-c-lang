package mycc

object PartialFunctionConversions {
  implicit class OrOps[A, B](`this`: PartialFunction[A, B]) {
    def |[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) = `this` orElse that
  }

  implicit class ToList[A, B](`this`: PartialFunction[A, B]) {
    def L :PartialFunction[A, List[B]] = `this` ->> { _ :: Nil }
  }

  implicit class ToReverse[A, B](`this`: PartialFunction[A, List[B]]) {
    def R[B1 >: B] :PartialFunction[A, List[B1]] = `this` ->> { _.reverse }
  }

  implicit class ToFMap[A, B](`this`: PartialFunction[A, List[B]]) {
    def >>-[A1 <: A, C](that: => B => List[C]): PartialFunction[A, List[C]] = `this` ->> { _.flatMap(that) }
  }

  implicit class ToEmpty[A, B](`this`: PartialFunction[A, B]) {
    def E: PartialFunction[A, List[Nothing]] = `this` ->> { _ => Nil }
  }

  implicit class Chain[A, B](`this`: PartialFunction[A, B]) {
    def ->>[C](f: B => C): PartialFunction[A, C] = `this` andThen f
  }

  implicit class Effectual[A, B](`this`: PartialFunction[A, B]) {
    def !!(effect: B => Unit): PartialFunction[A, B] = `this` ->> { x => effect(x) ; x }
  }
}