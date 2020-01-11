package sandbox.chapter3

final case class Box[A](value: A)

trait Printable[A] { self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = (value: B) => self.format(func(value))
}

object Printable {
  def apply[A](value: A)(implicit p: Printable[A]): String = p.format(value)
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = (value: String) => "\"" + value + "\""

  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"

  implicit def boxPrintable[A: Printable]: Printable[Box[A]] = implicitly[Printable[A]].contramap(_.value)
}

trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    def encode(value: B): String = self.encode(enc(value))
    def decode(value: String): B = dec(self.decode(value))
  }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)
}

object CodecInstances {
  implicit val stringCodec: Codec[String] = new Codec[String] {
    def encode(value: String): String = value

    def decode(value: String): String = value
  }
  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)
  implicit def boxCodec[A: Codec]: Codec[Box[A]] = implicitly[Codec[A]].imap(Box(_), _.value)
}
