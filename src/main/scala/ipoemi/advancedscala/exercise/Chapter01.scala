package ipoemi.advancedscala.exercise.chapter01

object `1.1.4` {

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

  object PrintableSyntax {

    implicit class PrintOps[A](a: A) {
      def format(implicit p: Printable[A]): String = p.format(a)
      def print(implicit p: Printable[A]): Unit = println(p.format(a))
    }

  }

}

object `1.2.5` {

  import cats._, cats.data._, cats.implicits._

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShowInstance = Show.show[Cat] { a =>
    s"${a.name} is a ${a.age} year-old ${a.color} cat."
  }
}

object `1.3.5` {

  import cats._, cats.data._, cats.implicits._

  final case class Cat(name: String, age: Int, color: String)

  implicit val catEqInstance = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.color === cat2.color
  }

  val cat1 = Cat("Garfield", 35, "orange and black")
  val cat2 = Cat("Heathcliff", 30, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  cat1 === cat2
  cat1 =!= cat2

  optionCat1 === optionCat2
  optionCat1 =!= optionCat2
}

object Chapter01Main {
  def main(args: Array[String]): Unit = {
    import `1.1.4`._
    import PrintableSyntax._

    val cat = Cat("a", 5, "red")

    Printable[Cat].format(cat)

    cat.format

    cat.print

  }
}

