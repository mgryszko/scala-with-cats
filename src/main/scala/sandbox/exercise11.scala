package sandbox.exercise11

import cats.kernel.{BoundedSemilattice, CommutativeMonoid, Monoid}
import cats.instances.map._
import cats.syntax.monoid._

trait GCounter[F[_,_],K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_,_], K, V](implicit counter: GCounter[F, K, V]) =
    counter

  implicit def mapInstance[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    override def increment(f: Map[K, V])(key: K, value: V)(implicit m: CommutativeMonoid[V]): Map[K, V] =
    f.updated(key, f.getOrElse(key, m.empty) |+| value)

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit bs: BoundedSemiLattice[V]): Map[K, V] =
    f1 |+| f2

    override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = m.combineAll(f.values)
  }
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

