package sandbox

import cats.kernel.Semigroup
import cats.syntax.semigroup._
import cats.syntax.either._

sealed trait Check[E, A] {
  def apply(value: A)(implicit s: Semigroup[E]): Either[E, A] = this match {
    case Pure(func) => func(value)
    case And(check1, check2) => (check1(value), check2(value)) match {
      case (Right(_), Right(_)) => value.asRight
      case (Right(_), Left(e)) => e.asLeft
      case (Left(e), Right(_)) => e.asLeft
      case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
    }
  }

  def and(that: Check[E, A]): Check[E, A] = And(this, that)
}

final case class Pure[E, A](func: A => Either[E, A]) extends Check[E, A]

final case class And[E, A](check1: Check[E, A], check2: Check[E, A]) extends Check[E, A]

