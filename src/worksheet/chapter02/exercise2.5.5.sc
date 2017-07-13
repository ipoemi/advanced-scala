import cats._, cats.data._, cats.implicits._

def add[A: Monoid](items: List[A]): A = items match {
  case Nil => Monoid[A].empty
  case x :: xs => x |+| add(xs)
}

add(List(1, 2, 3))

add(List(Some(1), None, Some(3)))

case class Order(totalCost: Double, quantity: Double)

implicit val orderMonoid = new Monoid[Order] {
  def empty: Order = Order(0, 0)
  def combine(x: Order, y: Order): Order =
    Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
}

add(List(Order(100, 2), Order(200, 2)))
