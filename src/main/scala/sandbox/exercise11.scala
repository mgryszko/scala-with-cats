package sandbox.exercise11

import cats.kernel.{BoundedSemilattice, CommutativeMonoid, Monoid}
import cats.instances.map._
import cats.syntax.monoid._

final case class GCounter[K, V](counters: Map[K, V]) {
  def increment(key: K, value: V)(implicit m: Monoid[V]): GCounter[K, V] =
    GCounter(counters.updated(key, counters.getOrElse(key, m.empty) |+| value))

  def merge(that: GCounter[K, V])(implicit bs: BoundedSemiLattice[V]): GCounter[K, V] =
    GCounter(counters |+| that.counters)

  def total(implicit m: CommutativeMonoid[V]): V = m.combineAll(counters.values)
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

