package ipoemi.advancedscala.exercise.chapter07

object `7.1.2` {
  val leftResult = (1 to 100).toList.foldLeft(List[Int]())((acc, x) => x :: acc)
  val rightResult = (1 to 100).toList.foldRight(List[Int]())(_ :: _)
}

object `7.1.3` {

  import cats._, cats.data._, cats.implicits._

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.foldRight(List.empty[B])((x, acc) => f(x) :: acc)

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    xs.foldRight(List.empty[B])((x, acc) => f(x) ++ acc)

  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    xs.foldRight(List.empty[A])((x, acc) => if (f(x)) x :: acc else acc)

  def sum[A: Monoid](xs: List[A]): A =
    xs.foldRight(Monoid[A].empty)((x, acc) => Monoid[A].combine(x, acc))
}

object `7.2.2.1` {

  import cats._, cats.data._, cats.implicits._

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum |@| func(item)).map(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  val r1 = listSequence(List(Vector(1, 2), Vector(3, 4)))
  val r2 = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

}

object `7.2.2.2` {

  import cats._, cats.data._, cats.implicits._
  import `7.2.2.1`._

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  val r3 = process(List(2, 4, 6))
  var r4 = process(List(1, 2, 3))
}

object `7.2.2.3` {

  import cats._, cats.data._, cats.implicits._
  import `7.2.2.1`._

  type ErrorsOr[A] = Validated[List[String], A]

  def process(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  val r5 = process(List(2, 4, 6))
  val r6 = process(List(1, 2, 3))
}

object Chapter07Main {
  def main(args: Array[String]): Unit = {
    import cats._, cats.data._, cats.implicits._

    import `7.1.2`._
    println(leftResult)
    println(rightResult)

    import `7.2.2.1`._
    println(r1)
    println(r2)

    import `7.2.2.2`._
    println(r3)
    println(r4)

    import `7.2.2.3`._
    println(r3)
    println(r4)

    println(Traverse[List].sequenceU(List(Vector(1, 2), Vector(3, 4))))
    println(Traverse[List].filter_(List(1, 2, 3))(_ % 2 == 0))
  }
}
