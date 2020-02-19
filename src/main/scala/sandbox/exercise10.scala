package sandbox

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.validated._
import cats.syntax.semigroup._

sealed trait Predicate[E, A] {
  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Predicate.Pure(func) => func(value)
    case Predicate.And(check1, check2) => (check1(value), check2(value)).mapN((_, _) => value)
    case Predicate.Or(check1, check2) => (check1(value), check2(value)) match {
      case (Valid(_), Valid(_)) => value.valid
      case (Valid(_), Invalid(_)) => value.valid
      case (Invalid(_), Valid(_)) => value.valid
      case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
    }
  }

  def and(that: Predicate[E, A]): Predicate[E, A] = Predicate.And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Predicate.Or(this, that)
}

object Predicate {
  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  final case class And[E, A](check1: Predicate[E, A], check2: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](check1: Predicate[E, A], check2: Predicate[E, A]) extends Predicate[E, A]
}

sealed trait Check[E, A, B] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] = Check.Map(this, func)

  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = Check.FlatMap(this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = Check.AndThen(this, that)
}

object Check {
  final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(a)
  }

  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).map(func)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = {
      check(a) match {
        case Valid(b) => func(b)(a)
        case Invalid(e) => e.invalid
      }
    }
  }

  final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(a).andThen(check2(_))
  }
}
