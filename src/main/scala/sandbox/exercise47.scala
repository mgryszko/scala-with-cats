package sandbox.chapter4

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object Factorial {
  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorialImpure(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorialImpure(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorial(n: Int): Logged[Int] = {
    for {
      fact <- slowly(if (n == 0) 1.pure[Logged] else factorial(n - 1).map(n * _))
      _ <- Vector(s"fact $n $fact").tell
    } yield fact
  }
}

