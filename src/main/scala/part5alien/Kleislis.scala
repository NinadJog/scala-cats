package part5alien

object Kleislis {

  /**
   * Summary
   *
   * Kleislis are a wrapper over functions that return higher-kinded instances
   * (i.e. functions that return F[_] or wrapper instances).
   *
   * They are useful for combining (composing/chaining) such functions using for
   * comprehensions and returning a single value at the end.
   *
   * Some convenience functions that Kleislis provide:
   * - map, flatMap
   * - andThen
   * - apply
   * - traverse
   *
   * Used for
   * - function composition when they return F[_] types
   * - dependency injection: Reader
   */

  // Examples of functions returning higher-kinded types
  val func1: Int => Option[String]  = x => if x % 2 == 0 then Some(s"$x is even") else None
  val func2: Int => Option[Int]     = x => Some(x * 3)

  /**
   * We would like to compose both of them and return something like
   * func3 = func2 andThen func1
   *
   * 'andThen' feeds the result of func2 as the input to func1. If only func1 and func2
   * didn't return Options, we would have been able to compose them using andThen as follows.
  */
  val plainFunc1: Int => String = x => if x % 2 == 0 then s"$x is even" else "fail"
  val plainFunc2: Int => Int    = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  // Here's the definition of a Kleisli:
  // final case class Kleisli[F[_], -A, B](run: A => F[B])
  import cats.data.Kleisli
  // import cats.instances.option._ // Was needed in older versions of Scala/Cats for FlatMap[Option]

  val func1K: Kleisli[Option, Int, String]  = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int]     = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String]  = func2K andThen func1K // Option 1. most idiomatic way

  // Kleisli relies on the fact that the wrapper type (in this case Option) has a flatMap.
  // We can manipulate the functions inside the Kleislis in a number of ways.
  // Convenience APIs:

  // multiply the result of func2K by 2
  val multiply = func2K.map(_ * 2) // equivalent to x => Option(...).map(_ * 2)

  // Option 2. Use flatMap directly
  val func3K_v2 = func2K.flatMap(_ => func1K) // same as func2K andThen func1K above

  // Option 3. >>= and explicit lambda (got it from DeepSeek A.I. when I asked it whether Option 1 & 2 were the same
  import cats.syntax.flatMap._
  val func3K_v3 = func2K >>= (x => func1K)

  // Out of all these, Option 1 (using andThen) is the most idiomatic: func2K andThen func1K

  //---------------------------------------------------------------------------
  // TODO: What does the following remind you of? Pattern: We wrap a few functions,
  // TODO: compose (chain) them and finally run them.

  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B]   // f: A => Id[B]

  // hint:
  val times2  = Kleisli[Id, Int, Int](x => x * 2)
  val plus4   = Kleisli[Id, Int, Int](x => x + 4)
  val composed =
    times2  flatMap (x =>
    plus4   map     (y => x + y))

  // The following two operations can be performed in parallel
  val composedFor = for {
    x <- times2
    y <- plus4
  } yield x + y
  // println(composedFor(3)) // 13

  // TODO Answer: This is similar to a dependency injection, similar to a Reader.
  // In fact if we replace Kleisli with Reader, we obtain identical results, as
  // shown below.

  import cats.data.Reader
  val times2_v2 = Reader[Int, Int](x => x * 2)
  val plus4_v2 = Reader[Int, Int](x => x + 4)

  val composedFor_v2 = for {
    x <- times2
    y <- plus4
  } yield x + y

  // TODO Answer 2: So Reader[A, B] is identical to Kleisli[Id, A, B]
  // TODO so InterestingKleisli is a Reader. In fact, Cats implements a Reader
  // TODO as a Kleisli.

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    // println(composedFor(3)) // 13
    println(composedFor_v2(3)) // 13
  }
}
