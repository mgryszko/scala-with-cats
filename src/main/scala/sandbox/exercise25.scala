package scalacats.chapter2

import cats.Monoid

object SuperAdder {
  case class Order(totalCost: Double, quantity: Double)

  import cats.instances.double._
  import cats.syntax.monoid._

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def empty: Order = Order(Monoid[Double].empty, Monoid[Double].empty)

    def combine(x: Order, y: Order): Order = Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
  }

  def add[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(Monoid[A].combine)
}
