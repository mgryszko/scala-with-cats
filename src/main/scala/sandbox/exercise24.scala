package sandbox.chapter2

object SetInstances {
  import cats.Monoid
  import cats.Semigroup

  implicit def unionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty

    def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
  }

  implicit def intersectionSemigroup[A]: Semigroup[Set[A]] = (x: Set[A], y: Set[A]) => x intersect  y

  implicit def symmetricDifferenceMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty

    def combine(x: Set[A], y: Set[A]): Set[A] = (x -- y) ++ (y -- x)
  }
}
