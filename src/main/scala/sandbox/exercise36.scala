package sandbox.chapter3

trait Printable[A] { self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = (value: B) => self.format(func(value))
}

object Printable {
  def apply[A](value: A)(implicit p: Printable[A]): String = p.format(value)
}

final case class Box[A](value: A)

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = (value: String) => "\"" + value + "\""

  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"

  implicit def boxPrintable[A: Printable]: Printable[Box[A]] =
    implicitly[Printable[A]].contramap(_.value)
}
