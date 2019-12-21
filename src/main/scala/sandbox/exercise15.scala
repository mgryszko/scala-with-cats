package sandbox

object EqInstances {
  import cats.Eq
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.eq._

  implicit val eqShow: Eq[Cat] = Eq.instance { (c1, c2) =>
    c1.name === c2.name && c1.age === c2.age && c2.color === c2.color
  }
}
