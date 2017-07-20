package ipoemi.advancedscala.exercise.chapter06

case class Cat(name: String, born: Int, color: String)
case class User(name: String, age: Int)

object `6.4.4` {

  import cats._, cats.data._, cats.implicits._

  type FormData = Map[String, String]
  type ErrorsOr[A] = Either[List[String], A]
  type AllErrorsOr[A] = Validated[List[String], A]

  def getValue[A](field: String)(formData: FormData): ErrorsOr[String] =
    formData.get(field).toRight(List(s"${field} must be specified"))

  /*
  def parseInt(s: String): ErrorsOr[Int] =
    Either.catchOnly[NumberFormatException](s.toInt).leftMap { _ =>
      List(s"${s} must be integer")
    }

  def nonBlank(name: String): ErrorsOr[String] =
    name.asRight[List[String]]
      .ensure(List(s"${name} must not be blank"))(!_.isEmpty)

  def nonNegative(n: Int): ErrorsOr[Int] =
    n.asRight[List[String]].ensure(List(s"${n} must be non-negative"))(_ >= 0)

  def readName(formData: FormData): ErrorsOr[String] =
    for {
      name <- getValue("name")(formData)
      validName <- nonBlank(name)
    } yield validName

  def readAge(formData: FormData): ErrorsOr[Int] =
    for {
      sAge <- getValue("age")(formData)
      age <- parseInt(sAge)
      validAge <- nonNegative(age)
    } yield validAge
  */

  def parseInt(name: String)(data: String): ErrorsOr[Int] =
    Either.catchOnly[NumberFormatException](data.toInt).leftMap { _ =>
      List(s"$name must be integer")
    }

  def nonBlank(name: String)(data: String): ErrorsOr[String] =
    Right(data).ensure(List(s"$name must not be blank"))(!_.isEmpty)

  def nonNegative(name: String)(data: Int): ErrorsOr[Int] =
    Right(data).ensure(List(s"$name must be non-negative"))(_ >= 0)

  def readName(formData: FormData): ErrorsOr[String] =
    getValue("name")(formData).flatMap(nonBlank("name"))

  def readAge(formData: FormData): ErrorsOr[Int] =
    getValue("age")(formData).flatMap(parseInt("age")).flatMap(nonNegative("age"))

  def readUser(formData: FormData): AllErrorsOr[User] = {
    val nameAndAge = readName(formData).toValidated |@| readAge(formData).toValidated
    nameAndAge.map(User.apply)
  }
}

object `6.3.4` {

  import scala.language.higherKinds
  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def product[M[_] : Monad, A, B](fa: M[A], fb: M[B]): M[(A, B)] =
  //Monad[M].flatMap(fa)(a => Monad[M].map(fb)(b => (a, b)))
    for {
      a <- fa
      b <- fb
    } yield (a, b)
}

object Chapter04Main {
  def main(args: Array[String]): Unit = {
    import cats._, cats.data._, cats.implicits._

    println(("Garfield".some |@| 1978.some |@| "Orange and black".some).map(Cat.apply))

    def catToTuple(cat: Cat) = (cat.name, cat.born, cat.color)

    implicit val catMonoid =
      (Monoid[String] |@| Monoid[Int] |@| Monoid[String]).imap(Cat.apply)(catToTuple)

    println(Monoid[Cat].empty)

    type ErrorsOr[A] = Either[Vector[String], A]

    Cartesian[ErrorsOr].product(
      Left({println(1); Vector("Error 1")}),
      Left({println(2); Vector("Error 2")})
    )

    for {
      x <- {println(1); Vector("Error 1")}.asLeft[Int]
      y <- {println(2); Vector("Error 2")}.asLeft[Int]
    } yield (x, y)

    println(Validated.catchOnly[NumberFormatException]("".toInt))

    val vectorCartesian = Vector(404).invalid[Int] |@| Vector(500).invalid[Int]
    println(vectorCartesian.tupled)
    println(vectorCartesian.map(_ + _))

    val vectorCartesian2 = Vector(404).invalid[Int] |@| 10.valid[Vector[Int]]
    vectorCartesian.tupled

    import `6.4.4`._

    println(readUser(Map("name" -> "T1", "age" -> "900")))
    println(readUser(Map("age" -> "-1")))

  }
}
