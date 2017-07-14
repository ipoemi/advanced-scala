package ipoemi.advancedscala.exercise.chapter03

import scala.util.Try

final case class Box[A](value: A)

object `3.5.4` {

  import cats._, cats.data._, cats.implicits._

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    implicit val treeFunctor = new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
      }
    }
  }

}

object `3.6.1.1` {

  trait Printable[A] {
    self =>
    def format(value: A): String
    def contramap[B](func: B => A) = new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
  }

  object Printable {
    def apply[A: Printable] = implicitly[Printable[A]]
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  implicit val booleanPrintable =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  implicit def boxPrintable[A: Printable] = Printable[A].contramap[Box[A]](_.value)
}

object `3.6.2.1` {
  trait Codec[A] {
    self =>
    def encode(value: A): String
    def decode(value: String): Option[A]

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      def encode(value: B): String = self.encode(enc(value))
      def decode(value: String): Option[B] = self.decode(value).map(dec)
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): Option[A] =
    c.decode(value)

  implicit val intCodec = new Codec[Int] {
    def encode(value: Int): String = value.toString
    def decode(value: String): Option[Int] =
      Try(value.toInt).toOption
  }

  implicit def boxCode[A](implicit C: Codec[A]) = C.imap[Box[A]](Box(_), _.value)
}

object Chapter03Main {
  def main(args: Array[String]): Unit = {
    import `3.5.4`._
    import cats.syntax.functor._

    val tree: Tree[Int] = Branch(Leaf(1), Leaf(2))
    println(tree.map(_ + 1))

    import `3.6.1.1`._

    println(format(Box(true)))

    import `3.6.2.1`._

    println(encode(Box(123)))

    println(decode[Box[Int]]("123"))

    import cats._, cats.data._, cats.implicits._

    implicit val symbolSemigroup: Semigroup[Symbol] =
      Semigroup[String].imap(Symbol.apply)(_.name)

    println('a |+| 'few |+| 'words)

  }
}

