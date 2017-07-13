import cats._, cats.data._, cats.implicits._

final case class Cat(name: String, age: Int, color: String)

implicit val catShowInstance = Show.show[Cat] { a =>
  s"${a.name} is a ${a.age} year-old ${a.color} cat."
}