package sandbox.exercise11

import cats.kernel.{BoundedSemilattice, CommutativeMonoid}
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.monoid._

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  def apply[F[_, _], K, V](implicit store: KeyValueStore[F]) =
    store

  implicit def mapInstance[K, V]: KeyValueStore[Map] = new KeyValueStore[Map] {
    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }
}

object GCounter {
  import KeyValueStore.KvsOps

  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
    counter

  implicit def gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], m: CommutativeMonoid[F[K, V]]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      override def increment(f: F[K, V])(key: K, value: V)(implicit m: CommutativeMonoid[V]): F[K, V] =
        f.put(key, f.getOrElse(key, m.empty) |+| value)

      override def merge(f1: F[K, V], f2: F[K, V])(implicit bs: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2

      override def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.combineAll
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

