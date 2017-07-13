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
