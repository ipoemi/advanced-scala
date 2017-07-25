package ipoemi.advancedscala.exercise.chapter10

import cats._, cats.data._, cats.implicits._, cats.data.Validated.{Valid, Invalid}

object `10.3` {
  case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(value: A): Either[E, A] = func(value)
    def and(that: CheckF[E, A])(implicit E: Monoid[E]): CheckF[E, A] = CheckF { a =>
      (this.func(a), that.func(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), _) => e.asLeft
        case (_, Left(e)) => e.asLeft
        case (Right(a1), Right(a2)) => a.asRight
      }
    }
  }

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) => func(a)
        case And(left, right) => (left(a) |@| right(a)).map((_, _) => a)
        /*
        case Or(left, right) => (left(a), right(a)) match {
          case (Valid(a), _) => Valid(a)
          case (_, Valid(a)) => Valid(a)
          case _ => (left(a) |@| right(a)).map((_, _) => a)
        }
        */
        case Or(left, right) => left(a) match {
          case Valid(a) => a.valid
          case Invalid(e1) => right(a) match {
            case Valid(a) => a.valid
            case Invalid(e2) => Invalid(e1 |+| e2)
          }
        }
      }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

}

object `10.4.2` {
  object predicate {
    sealed trait Predicate[E, A] {
      def and(that: Predicate[E, A]): Predicate[E, A] =
        And(this, that)

      def or(that: Predicate[E, A]): Predicate[E, A] =
        Or(this, that)

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        this match {
          case Pure(func) =>
            func(a)
          case And(left, right) =>
            (left(a) |@| right(a)).map((_, _) => a)
          case Or(left, right) =>
            left(a) match {
              case Valid(a1) => Valid(a)
              case Invalid(e1) =>
                right(a) match {
                  case Valid(a2) => Valid(a)
                  case Invalid(e2) => Invalid(e1 |+| e2)
                }
            }
        }
    }

    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  }

  import predicate.Predicate

  sealed trait Check[E, A, B] {
    def apply(in: A)(implicit A: Semigroup[E]): Validated[E, B]

    def map[C](func: B => C): Check[E, A, C] =
      Map(this, func)

    def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
      FlatMap(this, func)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen(this, that)
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)
  }

  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
    def apply(in: A)(implicit E: Semigroup[E]): Validated[E, C] =
      check(in).map(func)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(in: A)(implicit E: Semigroup[E]): Validated[E, C] =
    /*
    check(in) match {
      case Valid(a) => func(a)(in)
      case value@Invalid(e) => value
    }
    */
      check(in).withEither(_.flatMap(x => func(x)(in).toEither))
  }

  final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
    def apply(in: A)(implicit A: Semigroup[E]): Validated[E, C] =
      check1(in).withEither(_.flatMap(check2(_).toEither))
  }

  final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(in: A)(implicit A: Semigroup[E]): Validated[E, A] = pred(in)
  }
}

object `10.4.3` {

  sealed trait Predicate[E, A] {

    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          (left(a) |@| right(a)).map((_, _) => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(a1) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a2) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  object Predicate {

    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

    def lift[E, A](error: E, func: A => Boolean): Predicate[E, A] = Pure(a => if (func(a)) a.valid else error.invalid)
  }

  sealed trait Check[E, A, B] {

    import Check._

    def apply(in: A)(implicit A: Semigroup[E]): Validated[E, B]

    def map[C](func: B => C): Check[E, A, C] =
      Map(this, func)

    def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
      FlatMap(this, func)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen(this, that)
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)

    def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] = Pure(func)

    final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
      def apply(in: A)(implicit E: Semigroup[E]): Validated[E, C] =
        check(in).map(func)
    }

    final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
      def apply(in: A)(implicit E: Semigroup[E]): Validated[E, C] =
        check(in).withEither(_.flatMap(x => func(x)(in).toEither))
    }

    final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
      def apply(in: A)(implicit A: Semigroup[E]): Validated[E, C] =
        check1(in).withEither(_.flatMap(check2(_).toEither))
    }

    final case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
      def apply(in: A)(implicit A: Semigroup[E]): Validated[E, A] = pred(in)
    }

    final case class Pure[E, A, B](func: A => Validated[E, B]) extends Check[E, A, B] {
      def apply(in: A)(implicit A: Semigroup[E]): Validated[E, B] = func(in)
    }
  }

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  val checkUserName: Check[Errors, String, String] = Check(longerThan(4) and alphanumeric)

  def splitEmail: Check[Errors, String, (String, String)] =
    Check(_.split('@') match {
      case Array(name, domain) => (name, domain).validNel[String]
      case _ => "Must contain a single @ character".invalidNel[(String, String)]
    })

  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))

  val checkRight: Check[Errors, String, String] = Check(longerThan(3) and contains('.'))

  val joinEmail: Check[Errors, (String, String), String] =
    Check { case (l, r) => (checkLeft(l) |@| checkRight(r)).map(_ + '@' + _) }

  val checkEmail: Check[Errors, String, String] =
    splitEmail andThen joinEmail

}

object `10.5` {
  sealed trait Predicate[E, A] {

    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          (left(a) |@| right(a)).map((_, _) => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(a1) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a2) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }

    def run(implicit s: Semigroup[E]): A => Either[E, A] =
      (a: A) => this (a).toEither
  }

  object Predicate {

    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

    def lift[E, A](error: E, func: A => Boolean): Predicate[E, A] = Pure(a => if (func(a)) a.valid else error.invalid)
  }

  type Errors = NonEmptyList[String]

  type Result[A] = Either[Errors, A]

  type Check[A, B] = Kleisli[Result, A, B]

  // Create a check from a function:
  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  // Create a check from a Predicate:
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  val checkUserName: Check[String, String] = checkPred(longerThan(4) and alphanumeric)

  def splitEmail: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(name, domain) => (name, domain).asRight
      case _ => error("Must contain a single @ character").asLeft
    })

  val checkLeft: Check[String, String] = checkPred(longerThan(0))

  val checkRight: Check[String, String] = checkPred(longerThan(3) and contains('.'))

  val joinEmail: Check[(String, String), String] =
    check { case (l, r) => (checkLeft(l) |@| checkRight(r)).map(_ + '@' + _) }

  val checkEmail: Check[String, String] = splitEmail andThen joinEmail
}

object Chapter10Main {
  def main(args: Array[String]): Unit = {
    import `10.3`._

    val a: CheckF[List[String], Int] = CheckF { v => if (v > 2) v.asRight else List("Must be > 2").asLeft }
    val b: CheckF[List[String], Int] = CheckF { v => if (v < -2) v.asRight else List("Must be < -2").asLeft }
    val check = a and b
    println(check(5))
    println(check(0))

    val c: Check[List[String], Int] = Pure { v => if (v > 2) v.valid else List("Must be > 2").invalid }
    val d: Check[List[String], Int] = Pure { v => if (v < -2) v.valid else List("Must be < -2").invalid }
    val check2 = c and d
    println(check2(5))
    println(check2(0))

    println(List("123").invalid[Int].withEither(_.map(_ => 1.asRight)))

    //import `10.4.3`._
    import `10.5`._

    println((longerThan(3) and alphanumeric)("a@@"))
    println((error("Error1").invalid[Int] |@| error("Error2").invalid[Int]).map((_, _) => 2))

    println(checkUserName("123"))
    println(checkUserName("1234123aaa"))

    case class User(userName: String, email: String)

    def createUser(userName: String, email: String): Validated[Errors, User] =
      (checkUserName(userName).toValidated |@| checkEmail(email).toValidated).map(User.apply)

    println(createUser("tes", "testusertest.com"))
  }
}
