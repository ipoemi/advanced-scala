package ipoemi.advancedscala.exercise.chapter02

object `2.3` {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) = monoid
  }

  implicit val boolOrMonoid = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val boolAndMonoid = new Monoid[Boolean] {
    def empty: Boolean = true
    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val boolXorMonoid = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }

  implicit val boolXnorMonoid = new Monoid[Boolean] {
    def empty: Boolean = true
    def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
  }
}

object `2.4` {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) = monoid
  }


  implicit def setUnionMonoid[A] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty[A]
    def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  val intSetMonoid = Monoid[Set[Int]]

  intSetMonoid.combine(Set(1, 2), Set(2, 3))

  implicit def setIntersectionSemigroup[A] = new Semigroup[Set[A]] {
    def combine(a: Set[A], b: Set[A]) = a intersect b
  }
}

object `2.5.5` {

  import cats._, cats.data._, cats.implicits._

  def add[A: Monoid](items: List[A]): A = items match {
    case Nil => Monoid[A].empty
    case x :: xs => x |+| add(xs)
  }

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid = new Monoid[Order] {
    def empty: Order = Order(0, 0)
    def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

}


object Chapter02Main {
  def main(args: Array[String]): Unit = {
    import cats.instances.int._
    import cats.instances.option._

    import `2.5.5`._

    add(List(1, 2, 3))

    add(List(Some(1), None, Some(3)))

    add(List(Order(100, 2), Order(200, 2)))

  }
}

