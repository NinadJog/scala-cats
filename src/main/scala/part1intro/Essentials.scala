package part1intro

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // Companion object of trait Carnivore. Implement type-specific methods here
  object Carnivore

  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Crunch!")
  }

  // generics
  class MyList[A]

  // method notation
  val three         = 1 + 2
  val anotherThree  = 1.+(2)

  // functional programming
  val incrementer: Int => Int = x => x + 1    // anonymous function on RHS
  val anotherIncrementer: Int => Int = _ + 1

  // higher-order functions: map, flatMap, filter
  val processedList = List(1, 2, 3) map incrementer // List (2, 3, 4)
  val aLongerList = List(1, 2, 3) flatMap (x => List (x, x + 1))  // List(1,2, 2,3, 3,4)

  // for-comprehensions
  // The 'n' refers to each number from List(1, 2, 3), while 'c' refers to each character from List('a', 'b', 'c')
  val checkerboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c)))

  val checkerboard2 = List(1, 2, 3)       flatMap (n =>   // Stylized version; easier to read
                      List('a', 'b', 'c') map     (c =>
                        (n, c)))
  val checkerboard3 = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  // Options and Try
  val anOption: Option[Int] = Option(3) // The apply method of the companion object is being called
  // but the apply method returns a SUBTYPE of the option instance, the Some subtype.
  val doubledOption: Option[Int] = anOption map (_ * 2)

  // Try also has the map, flatMap, filter methods
  val anAttempt: Try[Int]         = Try(42)   // The apply method creates a Success(42)
  val aModifiedAttempt: Try[Int]  = anAttempt map (_ + 10) // Success(52)

  // pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match {
    case Some(value)  => s"The option is: $value"
    case None         => "The option is empty"
  }

  // Futures
  // Futures are data structures whose values are computed on some other thread at some point in the future
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // We can spawn a Future by calling the apply method of the Future type's companion object
  val aFuture: Future[Int] = Future {
    // quite a bit of code
    42
  }

  // wait for completion (async). This is one way of working on a Future
  aFuture.onComplete{   // Contains a partial function
    case Success(value)     => println(s"The async meaning of life is: $value")
    case Failure(exception) => println(s"The meaning of life failed with exception $exception")
  }

  // Another way to operate upon a future is by using map, flatMap, etc.
  val anotherFuture: Future[Int] = aFuture map (_ + 1)  // Will contain the value Future(43) when it completes

  // partial functions
  // They do not accept any random value from the domain (Int in the following example);
  // they accept only those values that correspond to the pattern match. If we pass any
  // other value, we will get a Pattern Match error
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1    => 43
    case 8    => 56
    case 100  => 999
  }

  // Advanced stuff: higher kinded types
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  // Notice that the type parameter is just List, not List[Int], List[String], etc.
  // That's because SequenceChecker takes a higher-kinded type, a one-hole type
  // such as List, Option, Try, etc.
  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {

  }
}
