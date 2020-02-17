package sandbox

import cats.data.Validated
import cats.kernel.Semigroup
import cats.syntax.apply._

sealed trait Check[E, A] {
  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) => func(value)
    case And(check1, check2) => (check1(value), check2(value)).mapN((_, _) => value)
  }

  def and(that: Check[E, A]): Check[E, A] = And(this, that)
}

final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

final case class And[E, A](check1: Check[E, A], check2: Check[E, A]) extends Check[E, A]

