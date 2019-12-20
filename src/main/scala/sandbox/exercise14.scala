package sandbox

object ShowInstances {
  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val catShow: Show[Cat] = Show.show { value =>
    s"${value.name.show} is a ${value.age.show} year-old ${value.color.show} cat."
  }
}
