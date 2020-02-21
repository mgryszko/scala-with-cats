package scalacats.chapter5

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Autobots {
  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] = {
    val powerLevel = powerLevels.get(autobot)
      .fold(s"$autobot power level not found".asLeft[Int])(_.asRight[String])
    EitherT(Future.successful(powerLevel))
  }

  private val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    powerLevel1 <- getPowerLevel(ally1)
    powerLevel2 <- getPowerLevel(ally2)
  } yield (powerLevel1 + powerLevel2 > 15)

  def tacticalReport(ally1: String, ally2: String): String = {
    val f = canSpecialMove(ally1, ally2).value
    val value = Await.result(f, 15.seconds)
    value.fold(error => error, if (_) s"$ally1 and $ally2 are ready to roll out!" else s"$ally1 and $ally2 need a recharge" )
  }
}
