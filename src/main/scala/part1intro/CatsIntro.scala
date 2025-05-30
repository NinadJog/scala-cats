package part1intro

object CatsIntro {

  // Eq
  // Enforces type-safe comparisons
  // The Eq typeclass was invented because the following code compiled in older
  // versions of Scala even though the Int and String are of different types.
  // In Eq we have an extension method == which makes sure the code compiles
  // only when two types are the same

  // val aComparison = 2 == "a string"

  // part 1 - type class import
  import cats.Eq

  // part 2 - import type class instances for the types we need (Int here)
  import cats.instances.int._

  // part 3 - use the type class API.
  val intEquality = Eq[Int]

  val aTypeSafeComparision = intEquality.eqv(2, 3) // false. eqv stands for equivalence
  // val anUnsafeComparision = intEquality.eqv(2, "a string") // does not even compile because types differ

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._ // bring into scope all the extension methods that Eq supports

  // The === is an extension method that works in the presence of Eq[Int]
  val anotherTypeSafeComparision = 2 === 3  // false.
  val neqComparison = 2 =!= 3 // true
  // val invalidComparision = 2 === "a string" // does not compile
  // extension methods are visible only in the presence of the correct type class instance

  // part 5 - extending the type class operations to composite types (lists, options, etc.)
  import cats.instances.list._  // brings into scope Eq[List[Int]]
  val aListComparison = List(2) === List(3) // false

  // part 6. Create a type class instance for a custom time that's not
  // automatically supported by Cats
  case class ToyCar(model: String, price: Double)

  given toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] {  // instance is a method on the Eq companion object
    (car1, car2) => car1.price == car2.price
  }

  // The triple equal === is valid for ToyCars in the presence of the given toyCarEq instance
  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99) // true

  /* Summary of Cats Imports
    import cats.YourTypeClass
    import cats.instances.yourType._
    import cats.syntax.yourTypeClass

    If the imports are confusing or the code doesn't compile, use the
    following two imports to import all.
    import cats._
    import cats.implicits._
   */

  def main(args: Array[String]): Unit = {
    println(compareTwoToyCars)  // true
  }
 }
