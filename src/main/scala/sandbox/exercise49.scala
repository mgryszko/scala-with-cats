package sandbox.chapter4

import cats.data.State
import cats.data.State._
import cats.syntax.applicative._

import scala.util.Try

object RPN {
  type CalcState[A] = State[List[Int], A]

  def evalInput(input: String): Int =
    evalAll(input.map(_.toString).toList).runA(Nil).value


  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState])((state, sym) => state.flatMap(_ => evalOne(sym)))
  }

  def evalOne(sym: String): CalcState[Int] = {
    for {
      oldStack <- get[List[Int]]
      (newStack, result) = evaluate(sym, oldStack)
      _ <- set[List[Int]](newStack)
    } yield result
  }

  private def evaluate(sym: String, stack: List[Int])=
    if (isNumber(sym))
      (sym.toInt :: stack, sym.toInt)
    else if (isOperator(sym)) {
      stack match {
        case op1 :: op2 :: newStack =>
          val result = calculate(sym, op1, op2)
          (result :: newStack, result)
        case _ => throw new IllegalStateException(s"stack too shallow: $stack")
      }
    } else {
      throw new IllegalStateException(s"unsupported symbol: $sym")
    }

  private def isNumber(sym: String) = Try(sym.toInt).isSuccess

  private def isOperator(sym: String): Boolean = sym == "+" || sym == "-" || sym == "*" || sym == "/"

  private def calculate(sym: String, op1: Int, op2: Int): Int = sym match {
    case "+" => op1 + op2
    case "-" => op1 - op2
    case "*" => op1 * op2
    case "/" => op1 / op2
    case _ => throw new IllegalStateException(s"unsupported symbol: $sym")
  }
}

