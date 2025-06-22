package part2abstractMath

// Monoid API
// empty  (the identity or zero)
// |+| from Semigroup

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._  // imports |+| extension method

  val numbers = (1 to 1000).toList

  // Semigroup's |+| is always associative, so sumLeft and sumRight have identical values.
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // to define a general fold API, we need a starting value, a "zero".
  // Semigroup does not provide that.
  // def combineFold[T: Semigroup](list: List[T]): T =
  //  list.foldLeft()(_ |+| _)

  // Monoids
  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999)
  val zero = intMonoid.empty // 0

  import cats.instances.option._
  val emptyOption: Option[Int] = Monoid[Option[Int]].empty            // None
  val combineOption = Monoid[Option[Int]].combine(Some(2), None)      // Some(2)
  val combineOption2 = Monoid[Option[Int]].combine(Some(2), Some(4))  // Some(6)

  //---------------------------------------------------------------------------
  // Exercise 1: Implement the reduceFold
  def combineFold[T: Monoid](list: List[T]): T =
    list.foldLeft(Monoid[T].empty)(_ |+| _)

  // Exercise 2: Combine a list of phonebooks
  val phonebooks = List(
    Map("Alice" -> 234, "Bob" -> 647),
    Map("Charlie" -> 389, "Daniel" -> 889),
    Map("Tina" -> 123)
  )

  // Solution:
  import cats.instances.map._ // imports a monoid of a Map that has the empty element
  val combinedPhoneBooks: Map[String, Int] = combineFold(phonebooks)

  //---------------------------------------------------------------------------
  // Exercise 3. Shopping carts and online stores with Monoids

  case class ShoppingCart(items: List[String], total: Double)

  given shoppingMonoid: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart] (
      ShoppingCart(List(), 0),  // empty shopping cart
      (s1, s2) => ShoppingCart(s1.items |+| s2.items, s1.total |+| s2.total)
      // instructor used (s1.items ++ s2.items, s1.total + s2.total). Result is same
    )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combinedPhoneBooks)

    // Test exercise 3
    println(checkout(List(
      ShoppingCart(List("iphone", "shoes"), 800),
      ShoppingCart(List("TV"), 200),
      ShoppingCart(List(), 0)
    )))
  }
}
