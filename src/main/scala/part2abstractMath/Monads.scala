package part2abstractMath

object Monads {

  /*
    Pattern
    - wrapping a value into a monadic value
    - the flatMap mechanism that transforms a monadic value into another sequentially

    MONAD is the Cats typeclass that embodies these two operations.
   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO implement map in terms of flatMap and pure
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._
  val optionMonad = Monad[Option]
  val anOption = optionMonad pure 42 // Option(42) == Some(42)

  // Transform a monad into another monad using flatMap
  val transformedOption = optionMonad.flatMap(anOption){ x => if x % 3 == 0 then Some(x + 1) else None}

  // generalize
  def getPairs[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    Monad[M].flatMap(ma)(a =>
    Monad[M].map(mb)    (b => (a, b)))

  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  val numberOption = Option(4)
  val charOption = Option('c')

  // extension methods
  import cats.syntax.applicative._  // pure extension method
  val oneOption = 1.pure[Option]    // type is Option[Int]. Wraps 1 into an Option
  val oneList   = 1.pure[List]      // List(1)

  import cats.syntax.flatMap._      // flatMap extension method
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option]) // Some(2)

  // Monads extend functors
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // for comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO: Write a shorter version of getPairs using for comprehension
  import cats.syntax.functor._  // map extension method
  def getPairsFor[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b) // same as ma.flatMap(a => mb.map(b => (a,b))
  
  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption)) // Some((4,c))
  }
}
