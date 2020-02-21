package scalacats.chapter6

import cats.data.Validated
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.either._

object Validation {
  case class User(name: String, age: Int)

  type AllErrorsOr[A] = Validated[List[String], A]

  def validate(params: Map[String, String]): Validated[List[String], User] = {
    (
      readName(params).toValidated,
      readAge(params).toValidated
    ).mapN(User)
  }

  def readName(params: Map[String, String]): Either[List[String], String] =
    getValue(params, "name")
      .flatMap(nonBlank)

  def readAge(params: Map[String, String]): Either[List[String], Int] =
    getValue(params, "age")
      .flatMap(parseInt)
      .flatMap(nonNegative)

  private def getValue(map: Map[String, String], key: String) =
    map.get(key).toRight(List(s"No $key specified"))

  private def parseInt(s: String): Either[List[String], Int] =
    Either.catchOnly[NumberFormatException](s.toInt).leftMap(_ => List(s"$s is not a number"))

  private def nonBlank(s: String): Either[List[String], String] =
    Right(s).ensure(List("Name empty"))(_.nonEmpty)

  private def nonNegative(n: Int): Either[List[String], Int] =
    Right(n).ensure(List("Age negative empty"))(_ >= 0)
}
