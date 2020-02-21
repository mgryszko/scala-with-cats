package sandbox.exercise11

import cats.kernel.{BoundedSemilattice, CommutativeMonoid}

final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCounter =
    GCounter(counters.updated(machine, counters.getOrElse(machine, 0) + amount))

  def merge(that: GCounter): GCounter =
    GCounter((counters.keySet ++ that.counters.keySet)
      .map(key => key -> (counters.getOrElse(key, 0) max that.counters.getOrElse(key, 0)))
      .toMap)

  def total: Int = counters.values.sum
}

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A

  def empty: A
}

object BoundedSemiLattice {
  implicit def setInstance[A](): BoundedSemilattice[Set[A]] = new BoundedSemilattice[Set[A]] {
    def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

    def empty: Set[A] = Set.empty
  }

  implicit val intInstance: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
    def combine(a1: Int, a2: Int): Int = a1 max a2

    def empty: Int = 0
  }
}

