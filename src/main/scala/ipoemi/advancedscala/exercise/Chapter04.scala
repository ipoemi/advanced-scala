package ipoemi.advancedscala.exercise.chapter04

object `4.1.2` {

  import scala.language.higherKinds

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(x => pure(func(x)))
  }

}

object `4.3.1` {

  import cats.{Id, Monad}

  val IdMonad = new Monad[Id] {
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
    def pure[A](x: A): Id[A] = x
    def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(a) match {
      case Left(a) => tailRecM(a)(f)
      case Right(a) => a
    }
  }

}

object `4.5.5` {

  import cats.Eval

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def foldRightEval(xs: List[A], accEval: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      xs match {
        case head :: tail => Eval.defer(f(head, foldRightEval(tail, accEval)(f)))
        case Nil => accEval
      }
    foldRightEval(as, Eval.now(acc))((x, eval) => eval.map(fn(x, _))).value
  }
}

object `4.6.3` {

  import cats._, cats.data._, cats.implicits._

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    /*
    val ans = if (n == 0) 1.pure[Logged] else slowly(factorial(n - 1) map (_ * n))
    ans.flatMap { a => Vector(s"fact $n $a").tell.map (_ => a)}
    */
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly(factorial(n - 1) map (_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }
}

object `4.7.3` {

  import cats.data.Reader
  import cats.data.Kleisli
  import cats.syntax.applicative._

  case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
  //Reader(_.passwords.get(username).map(_ == password).getOrElse(false))
    Reader(_.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    optUsername <- findUsername(userId)
    found <- optUsername.map { username =>
      checkPassword(username, password)
    }.getOrElse(false.pure[DbReader])
  } yield found

}

object `4.8.3` {

  import cats.data.State
  import cats.syntax.applicative._

  type CalcState[A] = State[List[Int], A]

  /*
  val operandConv: Map[String, (Int, Int) => Int] = Map(
    "+" -> (_ + _),
    "-" -> (_ - _),
    "*" -> (_ * _),
    "/" -> (_ / _)
  )

  def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { stack =>
    val newStack = if (operandConv.keySet.contains(sym)) {
      stack match {
        case r :: l :: tail => operandConv(sym) :: tail
        case _ => sys.error("error")
      }
    } else if (sym.matches("\\d".r)) {
      sym.toInt :: stack
    }
    (newStack, newStack.head)
  }
  */

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def operator(f: (Int, Int) => Int): CalcState[Int] = State { stack =>
    val nextStack = stack match {
      case r :: l :: tail => f(l, r) :: tail
      case _ => sys.error("error")
    }
    (nextStack, nextStack.head)
  }

  def operand(o: Int): CalcState[Int] = State(s => (o :: s, o))

  /*
  def evalAll(input: List[String]): CalcState[Int] = input match {
    case Nil => 0.pure[CalcState]
    case h :: t => evalOne(h) flatMap (_ => evalAll(t))
  }
  */

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (acc, x) =>
      acc.flatMap { _ => evalOne(x) }
    }

}

object `4.9.1` {

  import cats.Monad

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit def treeMonad = new Monad[Tree] {
    def pure[A](a: A): Tree[A] = Leaf(a)

    def flatMap[A, B](a: Tree[A])(f: A => Tree[B]): Tree[B] = a match {
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      case Leaf(v) => f(v)
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      f(a) match {
        case Branch(l, r) =>
          Branch(
            flatMap(l) {
              case Left(v) => tailRecM(v)(f)
              case Right(v) => pure(v)
            }, flatMap(r) {
              case Left(v) => tailRecM(v)(f)
              case Right(v) => pure(v)
            }
          )
        case Leaf(Left(v)) => tailRecM(v)(f)
        case Leaf(Right(v)) => pure(v)
      }
    }
  }

}

object Chapter04Main {
  def main(args: Array[String]): Unit = {
    import `4.5.5`._

    println(foldRight((1 to 100000).toList, 0)(_ + _))

    import cats._, cats.data._, cats.implicits._

    //println(Writer.tell(Vector(0)))
    println(Vector(0).tell)

    import `4.6.3`._

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    println(Await.result(Future.sequence(Vector(
      Future(factorial(5).run),
      Future(factorial(5).run)
    )), 5.seconds))

    import `4.7.3`._

    val db = Db(
      Map(
        1 -> "dade",
        2 -> "kate",
        3 -> "margo"
      ),
      Map(
        "dade" -> "zerocool",
        "kate" -> "acidburn",
        "margo" -> "secret"
      )
    )
    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(4, "davinci").run(db))

    import `4.8.3`._

    println(evalAll(List("1", "2", "+")).runA(Nil).value)
  }

}
