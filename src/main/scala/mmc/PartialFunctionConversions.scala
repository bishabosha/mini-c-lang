package mmc

object PartialFunctionConversions

  implicit final class OrOps[A, B](`this`: PartialFunction[A, B]) extends AnyVal
    def | [A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) = `this` orElse that

  implicit final class ToList[A, B](`this`: PartialFunction[A, B]) extends AnyVal
    def L: PartialFunction[A, List[B]] = `this` ->> { _ :: Nil }

  implicit final class ToReverse[A, B](`this`: PartialFunction[A, List[B]]) extends AnyVal
    def R [B1 >: B]: PartialFunction[A, List[B1]] = `this` ->> { _.reverse }

  implicit final class ToFMap[A, B](`this`: PartialFunction[A, List[B]]) extends AnyVal
    def >>- [A1 <: A, C](that: => B => List[C]): PartialFunction[A, List[C]] = `this` ->> { _.flatMap(that) }

  implicit final class ToEmpty[A, B](`this`: PartialFunction[A, B]) extends AnyVal
    def E: PartialFunction[A, List[Nothing]] = `this` ->> { _ => Nil }

  implicit final class Chain[A, B](`this`: PartialFunction[A, B]) extends AnyVal
    def ->> [C](f: B => C): PartialFunction[A, C] = `this` andThen f

  implicit final class Effectual[A, B](`this`: PartialFunction[A, B]) extends AnyVal
    def !!(effect: B => Unit): PartialFunction[A, B] = `this` ->> { x => effect(x) ; x }

