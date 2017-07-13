import cats._, cats.data._, cats.implicits._

final case class Cat(name: String, age: Int, color: String)

implicit val catEqInstance = Eq.instance[Cat] { (cat1, cat2) =>
  cat1.name === cat2.name &&
    cat1.age === cat2.age &&
    cat1.color === cat2.color
}

val cat1 = Cat("Garfield", 35, "orange and black")
val cat2 = Cat("Heathcliff", 30, "orange and black")

val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

cat1 === cat2
cat1 =!= cat2

optionCat1 === optionCat2
optionCat1 =!= optionCat2
