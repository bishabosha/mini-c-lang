package mmc

import scala.util.chaining._
import language.implicitConversions

object PartialFunctionConversions:

  implicit final class OrOps[A, B](self: PartialFunction[A, B]) extends AnyVal:
    def | [A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) = self orElse that

  implicit final class ToList[A, B](self: PartialFunction[A, B]) extends AnyVal:
    def L: PartialFunction[A, List[B]] = self ->> (_ :: Nil)

  implicit final class ToReverse[A, B](self: PartialFunction[A, List[B]]) extends AnyVal:
    def R [B1 >: B]: PartialFunction[A, List[B1]] = self ->> (_.reverse)

  implicit final class ToFMap[A, B](self: PartialFunction[A, List[B]]) extends AnyVal:
    def >>- [A1 <: A, C](that: => B => List[C]): PartialFunction[A, List[C]] = self ->> (_.flatMap(that))

  implicit final class ToEmpty[A, B](self: PartialFunction[A, B]) extends AnyVal:
    def E: PartialFunction[A, List[Nothing]] = self ->> (_ => Nil)

  implicit final class Chain[A, B](self: PartialFunction[A, B]) extends AnyVal:
    def ->> [C](f: B => C): PartialFunction[A, C] = self andThen f

  implicit final class Effectual[A, B](self: PartialFunction[A, B]) extends AnyVal:
    def !!(effect: B => Unit): PartialFunction[A, B] =
      self ->> (_.tap(effect))
