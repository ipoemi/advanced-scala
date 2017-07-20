package ipoemi.advancedscala.exercise.chapter05

object `5.3` {

  import cats.data.EitherT
  import cats.instances.future._
  import cats.syntax.either._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration.Duration

  //type Response[A] = Future[Either[String, A]]

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(x) => EitherT(Future(x.asRight[String]))
      case None => EitherT(Future(s"${autobot} is unreachable".asLeft[Int]))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      powerLevel1 <- getPowerLevel(ally1)
      powerLevel2 <- getPowerLevel(ally2)
    } yield powerLevel1 + powerLevel2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val result = canSpecialMove(ally1, ally2).value
    Await.result(result, Duration.Inf) match {
      case Left(x) => x
      case Right(false) => s"${ally1} and ${ally2} need a recharge."
      case Right(true) => s"${ally1} and ${ally2} are ready to roll out!"
    }

  }

}

object Chapter04Main {
  def main(args: Array[String]): Unit = {
    import cats.data._
    import cats.implicits._

    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration.Duration
    import scala.concurrent.ExecutionContext.Implicits.global

    type Error = String
    type ErrorOr[A] = Either[Error, A]
    type ErrorOptionOr[A] = OptionT[ErrorOr, A]

    type FutureEither[A] = EitherT[Future, Error, A]
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    println(1.pure[FutureEitherOption])

    import `5.3`._
    println(tacticalReport("Jazz", "Bumblebee"))
    println(tacticalReport("Bumblebee", "Hot Rod"))
    println(tacticalReport("Jazz", "Ironhide"))

  }

}
