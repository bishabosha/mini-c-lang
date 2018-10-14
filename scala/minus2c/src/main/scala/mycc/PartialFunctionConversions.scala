package mycc

object PartialFunctionConversions {
  implicit class OrOps[A, B](`this`: PartialFunction[A, B]) {
    def |[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) = `this` orElse that
  }

  implicit class ToList[A, B](`this`: PartialFunction[A, B]) {
    def L: PartialFunction[A, List[B]] = `this` andThen { _ :: Nil}
  }
}