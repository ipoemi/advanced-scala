package ipoemi.advancedscala.exercise.chapter09

import cats._, cats.data._, cats.implicits._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.Await

object `9.2` {
  def foldMap[A, B: Monoid](xs: Vector[A])(f: A => B): B =
    xs.foldLeft(Monoid[B].empty)(_ |+| f(_))
}

object `9.3.3` {

  import `9.2`._

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (values.size.toDouble / numCores).ceil.toInt
    val groupedValues = values.grouped(groupSize)
    /*
    val procs = groupedValues.map { xs =>
      Future {xs.foldLeft(Monoid[B].empty)(_ |+| func(_))}
    }.toList
    */
    val procs = groupedValues.map { xs =>
      Future(foldMap(xs)(func))
    }
    Future.sequence(procs).map { ys =>
      ys.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }
}

object `9.3.4` {

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (values.size.toDouble / numCores).ceil.toInt
    /*
    val groupedValues = values.grouped(groupSize)
    val procs = groupedValues.map { xs =>
      Future(Foldable[Vector].foldMap(xs)(func))
    }.toVector
    Future.sequence(procs).map(Foldable[Vector].fold(_))
    */
    values.grouped(groupSize).toVector.traverse(group => Future(group.foldMap(func))).map(_.combineAll)
  }
}

object Chapter09Main {
  def main(args: Array[String]): Unit = {
    println(Runtime.getRuntime.availableProcessors())

    import `9.3.3`._
    println(Await.result(parallelFoldMap(Vector(1, 2, 3))(identity), Duration.Inf))
    println(Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), Duration.Inf))
  }
}
