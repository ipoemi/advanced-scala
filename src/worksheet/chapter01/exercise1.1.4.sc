
trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {
  val intPrintableInstance = new Printable[Int] {
    def format(a: Int): String = a.toString
  }
  val stringPrintableInstance = new Printable[String] {
    def format(a: String): String = a
  }
}

object Printable {
  def apply[A](implicit A: Printable[A]) = A
  def format[A: Printable](a: A): String = Printable[A].format(a)
  def print[A: Printable](a: A): Unit = println(Printable[A].format(a))
}

final case class Cat(name: String, age: Int, color: String)

case object Cat {
  implicit val catPrintableInstance = new Printable[Cat] {
    def format(a: Cat): String =
      s"${a.name} is a ${a.age} year-old ${a.color} cat."
  }
}

val cat = Cat("a", 5, "red")

Printable[Cat].format(cat)

object PrintableSyntax {
  implicit class PrintOps[A](a: A) {
    def format(implicit p: Printable[A]): String = p.format(a)
    def print(implicit p: Printable[A]): Unit = println(p.format(a))
  }
}

import PrintableSyntax._

cat.format

cat.print
