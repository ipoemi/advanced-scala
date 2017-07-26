package ipoemi.advancedscala.exercise.chapter11

import cats._, cats.data._, cats.implicits._

object `11.2.3` {
  final case class GCounter(counters: Map[String, Int]) {

    def increment(machine: String, amount: Int) =
      GCounter(counters.updated(machine, counters.getOrElse(machine, 0) + amount))

    def get: Int = counters.values.sum

    def merge(that: GCounter): GCounter = {
      /*
      val keySet = counters.keySet ++ that.counters.keySet
      val keyValues = keySet.toVector.map { key =>
        key -> (counters.get(key).getOrElse(0) max that.counters.get(key, 0))
      }
      GCounter(Map(keyValues: _*))
      */
      GCounter(that.counters ++ (
        for {
          (key, value) <- counters
        } yield {
          key -> (value max that.counters.getOrElse(key, 0))
        })
      )
    }
  }
}

object `11.3.2.1` {
  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  object BoundedSemiLattice {
    implicit val intBoundedSemiLattice = new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int = a1 max a2
      def empty: Int = 0
    }

    implicit def setBoundedSemiLattice[A] = new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
      def empty: Set[A] = Set.empty[A]
    }
  }
}

object `11.3.2.2` {

  import `11.3.2.1`._
  import BoundedSemiLattice._

  final case class GCounter[A](counters: Map[String, A]) {

    def increment(machine: String, amount: A)(implicit M: Monoid[A]): GCounter[A] =
      //GCounter(counters.updated(machine, M.combine(counters.getOrElse(machine, M.empty), amount)))
      GCounter(counters.updated(machine, counters.getOrElse(machine, M.empty) |+| amount))

    def get(implicit M: Monoid[A]): A = counters.foldMap(identity)

    def merge(that: GCounter[A])(implicit B: BoundedSemiLattice[A]): GCounter[A] = {
      /*
      GCounter(that.counters ++ (
        for {
          (key, value) <- counters
        } yield {
          key -> B.combine(value, that.counters.getOrElse(key, B.empty))
        })
      )
      */
      GCounter(counters |+| that.counters)
    }
  }
}

object Chapter10Main {
  def main(args: Array[String]): Unit = {
    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("a" -> 11, "b" -> 12)
    println(map1 ++ map2)
    println(map1 + ("a" -> 12))

    import `11.3.2.2`._
    val c1 = GCounter(Map("a" -> 1, "b" -> 2))
    val c2 = GCounter(Map("a" -> 1, "b" -> 3))
    val c3 = c1.merge(c2)
    println(c3)
    println(c3.get)
  }
}
