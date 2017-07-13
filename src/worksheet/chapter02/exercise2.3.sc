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

