package sandbox

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.validated._
import cats.syntax.semigroup._

sealed trait Check[E, A] {
  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) => func(value)
    case And(check1, check2) => (check1(value), check2(value)).mapN((_, _) => value)
    case Or(check1, check2) => (check1(value), check2(value)) match {
      case (Valid(_), Valid(_)) => value.valid
      case (Valid(_), Invalid(_)) => value.valid
      case (Invalid(_), Valid(_)) => value.valid
      case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
    }
  }

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def or(that: Check[E, A]): Check[E, A] = Or(this, that)
}

final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

final case class And[E, A](check1: Check[E, A], check2: Check[E, A]) extends Check[E, A]

final case class Or[E, A](check1: Check[E, A], check2: Check[E, A]) extends Check[E, A]

