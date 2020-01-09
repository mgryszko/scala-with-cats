package sandbox.chapter2

object LogicalInstances {
  import cats.Monoid

  implicit val andMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val orMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val xorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }

  implicit val xnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = (x || !y) && (!x || y)
  }
}
