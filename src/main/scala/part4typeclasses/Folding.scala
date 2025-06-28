package part4typeclasses

import cats.{Eval, Monoid}
import cats.syntax.monoid.*

object Folding {

  // TODO as a warmup: Implement all these methods in terms of foldLeft or foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list
        .foldLeft(List.empty[B]) { (acc, a) => f(a) :: acc }
        .reverse

    // Alternative implementation with foldRight
    def map_v2[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B]) { (a, currentList) => f(a) :: currentList }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B]) { (currentList, a) => currentList ++ f(a) }

    // Better alternative: use a foldRight instead of currentList ++ f(a)
    // f(a) will be last in the end result in the following code and all the elements
    // of currentList will be before f(a) and in the same order.
    def flatMap_v2[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B]) { (currentList, a) =>
        currentList.foldRight(f(a))(_ :: _) }

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list
        .foldLeft(List.empty[A]){ (currentList, a) => if predicate(a) then a :: currentList else currentList }
        .reverse

    def filter_v2[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A]) { (a, currentList) =>
        if predicate(a) then a :: currentList else currentList }

    // We can use foldLeft or foldRight because the combination function should be associative
    def combineAll[A : Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(_ |+| _)

    // Alternative implementation using foldRight
    def combineAll_v2[A : Monoid](list: List[A]): A =
      list.foldRight(Monoid[A].empty)(Monoid[A].combine)
  }
  //---------------------------------------------------------------------------
  /*
    Since map, flatMap, filter, combineAll, etc. can all be implemented in terms of
    foldLeft or foldRight, Cats provides a dedicated type class called Foldable.

    Foldable is a higher-kinded type class that provides two fundamental methods:
    foldLeft and foldRight, and several other methods derived from them.
   */
  import cats.Foldable

  // Foldables for List
  import cats.instances.list._  // fetch given Foldable[List]
  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)  // 6

  // Foldables for Option
  import cats.instances.option._  // fetch Foldable[Option]
  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // Option(32)

  /*
    Foldables works similarly for other wrapper types. So they are useful for
    generalizable APIs where we might want to pass other wrapper types such as
    list or vector or lazy list, etc.
   */

  /*
    foldRight has a very different signature than foldLeft. Why does it use Eval?
    Because foldRight can be implemented using stack recursion, and using Eval
    makes everything stack-safe, regardless of how the container is being
    implemented. Using eval makes foldRight stack-safe.

    If the container is not stack safe, foldRight will make it stack-safe,
    because all it does is chain Evals together. When we evaluate the chained
    evals at the end, we get a stack-safe expression. foldRight makes any container
    stack-safe. That's one of the benefits of foldables.
  */
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) {
    (num, eval) => eval map (_ + num)
  }

  // ------ Convenience methods in the presence of of Monoid ------
  // Foldable also provides the combineAll method
  // import cats.instances.int._   // fetch Monoid[Int] needed for older versions of Scala/Cats
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3, 4))

  // Foldable also provides a convenience method called foldMap that maps
  // and then combines.
  // import cats.instances.string._ // needed by older versions of Scala/Cats for Monoid[String]
  val mappedConcat: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // "123"

  /*
    Convenience methods for deep traversals if we have multiple foldable data
    structures nested inside one another.

    We can combine the following two foldables -- a foldable of list and a foldable
    of vector -- and obtain a very powerful foldable that can then traverse all
    the elements regardless of how they were nested inside.
  */
  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val combined: Int = (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // extension methods
  import cats.syntax.foldable._
  val sum3: Int = List(1, 2, 3).combineAll  // needs Foldable[List], Monoid[Int]

  val mappedConcat2: String = List(1, 2, 3) foldMap (_.toString)
  
  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    import ListExercises._
    val numbers = (1 to 10).toList
    // println(map_v2(numbers)(_ + 1))
    // println(flatMap_v2(numbers)(x => (1 to x).toList))
    println(filter_v2(numbers)(_ % 2 == 0)) // List(2, 4, 6, 8, 10)

    println(combineAll(numbers))  // 55
    println(anotherSum)
    println(mappedConcat)
  }
}
