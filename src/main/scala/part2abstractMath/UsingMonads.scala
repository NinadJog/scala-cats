package part2abstractMath

object UsingMonads {

  import cats.Monad
  import cats.instances.list._

  // bring into scope the monad instance for list
  val monadList = Monad[List] // fetches the implicit Monad[List]

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T]   = Either[Throwable, T]

  // Illustrates use of pure and flatMap
  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither: LoadingOr[Int] = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading =
    loadingMonad.flatMap(anEither)
      { n => if n % 2 == 0 then Right(n + 1) else Left("Loading meaning of life") }

  //---------------------------------------------------------------------------
  // imaginary online store with 2 api methods
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if orderStatus.orderId > 1000 then
      Left("Not available yet, refreshing data...")
    else
      Right("Amsterdam, Netherlands")

  // To track an order location for a particular order id, we will have to call
  // both APIs and combine them together. Combining is applicable for a monad.
  val orderId = 456L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  // Alternative: call flatMap on the monadic value
  val orderLocation_v2: LoadingOr[String] = getOrderStatus(orderId) flatMap trackLocation

  // Another alternative: for comprehension
  // We don't need to import cats.syntax.xxx for map and flatMap extension methods,
  // as Either is part of Scala's standard library and Either is right-biased.
  val orderLocation_v3: LoadingOr[String] =
    for {
      orderStatus <- getOrderStatus(orderId)
      location    <- trackLocation(orderStatus)
    } yield location

  //---------------------------------------------------------------------------
  // TODO: The service layer of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  // api definition of the service layer
  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  /* Requirements
    - If host & port are found in the config map, return an M containing a
      connection with those values, otherwise method will fail according to the
      logic of the type M. (For Try it will return a Failure, for Option it will
      return None, for Future it will be a failed future, for Either it will be
      Left.)

    - The issueRequest method returns an M containing the string, "request (payload)
      has been accepted" if the payload is less than 20 characters otherwise the
      method will fail according to the logic of the type M.

    - TODO: Provide a real implementation of HttpService using one of Try, Option, Future, Either.
      Define a value of type HttpService that extends HttpService of Try or Either, etc.
   */

  //-----------------
  // My implementation with Either
  import java.util.NoSuchElementException
  object EitherHttpService extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] =
      try {
        val host = cfg("host")
        val port = cfg("port")
        Right(Connection(host, port))
      }
      catch
        case ex: NoSuchElementException => Left(s"No such host or port: $ex")

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] =
      if payload.length < 20 then Right(s"($payload) has been accepted")
      else Left("FAIL: payload is too long")
  }

  //-----------------
  // Implementation with Option
  object OptionHttpService extends HttpService[Option] {

    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")  // get returns an Option
        p <- cfg.get("port")
      } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None
      else Some(s"($payload) has been accepted")
  }

  //----------
  // high-level API
  // Since there are two implementations of the service, one with Option and the
  // other with Either (LoadingOr), we can write a method that can accept
  // either of the two services to get a response.
  // getResponse is a high-level API

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] =
    for {
      connection  <- service.getConnection(config)
      response    <- service.issueRequest(connection, payload)
    } yield response

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    // Test the Http service exercise with OptionHttpService
    val payload = "Hello, HTTP service"
    val responseOption: Option[String] = for {
      connection <- OptionHttpService.getConnection(config)
      response <- OptionHttpService.issueRequest(connection, payload)
    } yield response

    // same - written with flatMap
    val responseOption_v2: Option[String] =
      OptionHttpService.getConnection(config).flatMap(
        OptionHttpService.issueRequest(_, payload))

    println(responseOption)
    println(responseOption_v2)

    //---------------
    // Test the EitherHttpService
    val responseOption_v3: Either[String, String] = for {
      connection <- EitherHttpService.getConnection(config)
      response <- EitherHttpService.issueRequest(connection, payload)
    } yield response

    // same - written with flatMap
    val responseOption_v4: Either[String, String] =
      EitherHttpService.getConnection(config).flatMap(
        EitherHttpService.issueRequest(_, payload))

    println(responseOption_v3)
    println(responseOption_v4)

    // Test the generic method
    println(getResponse(OptionHttpService, "Hello Option"))
    println(getResponse(EitherHttpService, "Hello LoadingOr"))

  }
}
