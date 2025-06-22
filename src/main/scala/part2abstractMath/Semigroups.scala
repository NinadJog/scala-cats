package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._ // fetch implicit typeclass instances into scope

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination      = naturalIntSemigroup.combine(2, 3) // addition

  // general API
  def reduceThings[T](list: List[T])(using semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  def reduceThings_v2[T](list: List[T])(using Semigroup[T]): T =
    list.reduce(Semigroup[T].combine)

  // Semigroup as a type context
  def reduceThings_v3[T :  Semigroup](list: List[T]): T =
    list.reduce(Semigroup[T].combine)

  //---------------------------------------------------------------------------
  // Exercise 1: Create a Semigroup instance for the custom data type Expense
  case class Expense(id: Long, amount: Double)

  given expenseSemigroup: Semigroup[Expense] =
    Semigroup.instance[Expense] {
      (e1, e2) => Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
    }

  //---------------------------------------------------------------------------
  // extension methods from semigroup: |+|
  import cats.syntax.semigroup._
  val anIntSum          = 2 |+| 3 // requires the presence of a given (implicit) Semigroup[Int]
  val aStringConcat     = "we like " |+| "semigroups"
  val aCombinedExpense  = Expense(4, 80) |+| Expense(46, 56)

  // Exercise 2. Write the reduceThings method with |+|
  def reduceThings_v4[T: Semigroup](list: List[T]): T = list reduce (_ |+| _)

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    // List of Int
    val numbers = (1 to 10).toList
    println(reduceThings(numbers))    // 55
    println(reduceThings_v2(numbers)) // 55. Compiler injects Semigroup[Int]

    // List of Option of Int
    val numOptions: List[Option[Int]] = numbers map (Option(_))  // convert List[Int] to List[Option[Int]]
    import cats.instances.option._    // compiler will produce an implicit Semigroup[Option[Int]] since we have a Semigroup[Int] in scope
    println(reduceThings(numOptions)) // Some(55)

    // Test exercise 1
    val expenses = List(Expense(1, 99), Expense(2, 35), Expense(43, 11))
    val combinedExpenses = reduceThings_v4(expenses)
    println(combinedExpenses) // Expense(43,144.0)

  }
}
