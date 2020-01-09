package sandbox.chapter1

final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = (value: String) => value

  implicit val intPrintable: Printable[Int] = (value: Int) => value.toString

  implicit val catPrintable: Printable[Cat] = (value: Cat) =>
    s"${Printable.format(value.name)} is a ${Printable.format(value.age)} year-old ${Printable.format(value.color)} cat."
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String =
    printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]) {
    println(format(value))
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = Printable.format(value)

    def print(implicit printable: Printable[A]) {
      Printable.print(value)
    }
  }
}
