package scalacats.chapter10

import cats.data.{Kleisli, NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.validated._
import cats.syntax.semigroup._
import cats.instances.either._

sealed trait Predicate[E, A] {
  def run(implicit s: Semigroup[E]): A => Either[E, A] = value => apply(value).toEither

  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Predicate.Pure(func) => func(value)
    case Predicate.And(pred1, pred2) => (pred1(value), pred2(value)).mapN((_, _) => value)
    case Predicate.Or(pred1, pred2) => (pred1(value), pred2(value)) match {
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

  final case class And[E, A](pred1: Predicate[E, A], pred2: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](pred1: Predicate[E, A], pred2: Predicate[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
    Pure(f)

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if(fn(a)) a.valid else err.invalid)
}

object Checks {
  type Errors = NonEmptyList[String]
  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]

  def check[A, B](func: A => Result[B]): Check[A, B] = Kleisli(func)
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli[Result, A, A](pred.run)

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"), str => str.length > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(error(s"Must be all alphanumeric characters"), str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char"), str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char only once"), str => str.count(c => c == char) == 1)

  /*
• An email address must contain an @ sign. Split the string at the @. The string to the le􏰁 must not be empty. The string to the right must be at least three characters long and contain a dot.
   */
  val validUsername: Check[String, String] = checkPred(longerThan(3).and(alphanumeric))

  val validEmail: Check[String, String] =
    checkPred(containsOnce('@'))
      .map(_.split('@'))
      .andThen(parts => parts match {
        case Array(username, domain) =>
          (validEmailUsername(username), validEmailDomain(domain))
            .mapN(_ + "@" + _)
      })

  val validEmailUsername: Check[String, String] = checkPred(longerThan(0))
  val validEmailDomain: Check[String, String] = checkPred(longerThan(2).and(contains('.')))

  case class User(username: String, email: String)

  def createUser(username: String, email: String): Result[User] =
    (validUsername(username), validEmail(email)).mapN(User)
}
