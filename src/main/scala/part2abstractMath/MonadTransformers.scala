package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  /**
   * Monad transformers are higher-kinded types that provide convenience methods
   * to handle nested monadic values. They are NOT type classes.
   *
   * They allow us to use maps and flatMaps on nested monads without needing to
   * unwrap the inner monads all the time.
   *
   * Motivation:
   * How do we perform the following sum without manually unwrapping all ints?
   */
  def sumAllOptions(list: List[Option[Int]]): Int = ???

  // option transformer
  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List] i.e. Monad[List]

  /* From the Cats documentation of OptionT:
    OptionT[F[_], A] is a light wrapper on F[Option[A]] with some convenient
    methods. It's a case class containing values of type A:

    case class OptionT[F[_], A](value: F[Option[A]]) {...}

    In the above example, F is List.
   */

  // OptionT[List, Int] means List[Option[Int]]
  // We are wrapping a list of options into the higher-kinded OptionT type.
  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))

  // We can also specify the last element of the following list as  Option.empty[Char]
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty))

  /*
   * If we were to combine the two lists to form a cross product containing tuples
   * of number and char, we would have to unwrap all the options and wrap the
   * tuples back into them. That would be clunky. This is where OptionT helps.
   *
   * We can use a for comprehension as though List[Option[_]] were a single monad.
   */
  val listOfTuples: OptionT[List, (Int, Char)] =
    for {
      num  <- listOfNumberOptions
      char <- listOfCharOptions
    } yield (num, char)

  /**
   * A monad transformer provides a convenience API. It provides map and flatMap
   * methods so that when we have a wrapper monad (say List) over our own monad
   * (say Option), we don't need to unwrap the inner monad all the time.
   */

  // either transformer
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] =
    EitherT(List(Left("something wrong"), Right(42), Right(2)))

  given ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future(Right(45)))

  // alternative way to define it using EitherT's convenience functions left and right
  val futureOfEither_v2: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45))

  //---------------------------------------------------------------------------
  // TODO Exercise 1
  /**
   * There's a multi-machine cluster that will receive a traffic surge following a media
   * appearance. We measure bandwidth in units. We want to allocate TWO of the servers
   * to cope with the traffic spike. The current capacity of each server is known and
   * we know that we'll hold the traffic if the sum of bandwidths is > 250
   */

  val bandwidths = Map(
    "server1.xxx.com" -> 50,
    "server2.xxx.com" -> 300,
    "server3.xxx.com" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T]  // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match {
      case None     => EitherT(Future(Left(s"server $server unreachable")))
      case Some(b)  => EitherT(Future(Right(b)))
    }

  // Alternative version. Same logic, but more elegant code as it uses EitherT's  left and right methods
  def getBandwidth_v2(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match {
      case None     => EitherT.left(Future(s"server $server unreachable"))
      case Some(b)  => EitherT.right(Future(b))
    }

  // TODO 1
  // hint: call bandwidth twice and combine the results
  import cats.instances.future._  // fetch Monad[Future]
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      b1 <- getBandwidth(s1)  // b1 and b2 have type Int
      b2 <- getBandwidth(s2)
    } yield (b1 + b2) > 250 // return type is Future[Either[String, Boolean]]

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform { // transform Future[Either[String, Boolean]] to Future[Either[String, String]
      case Left(reason) => Left(s"Servers $s1 and $s2 cannot cope with the traffic spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 don't have enough total bandwidth to cope with the incoming spike")
      case Right(true)  => Right(s"Servers $s1 and $s2 can cope with the incoming spike")
    }

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(listOfTuples.value) // value accesses the Options inside
    // List(Some((1,a)), Some((1,b)), None, Some((2,a)), Some((2,b)), None)

    // Test Exercise 1
    val resultFuture: Future[Either[String, String]] =
      generateTrafficSpikeReport("server1.xxx.com", "server3.xxx.com").value
    resultFuture foreach println
  }
}
