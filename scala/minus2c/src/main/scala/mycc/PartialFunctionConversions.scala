package mycc

object PartialFunctionConversions {
  implicit class OrOps[A, B](`this`: PartialFunction[A, B]) {
    def |[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) = `this` orElse that
  }

  implicit class ToList[A, B](`this`: PartialFunction[A, B]) {
    def L :PartialFunction[A, List[B]] = `this` ->> { _ :: Nil}
  }

  implicit class ToEmpty[A, B](`this`: PartialFunction[A, B]) {
    def E: PartialFunction[A, List[Nothing]] = `this` ->> { _ => Nil}
  }

  implicit class Chain[A, B](`this`: PartialFunction[A, B]) {
    def ->>[C](f: B => C): PartialFunction[A, C] = `this` andThen f
  }
}