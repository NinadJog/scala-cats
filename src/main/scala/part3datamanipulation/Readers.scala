package part3datamanipulation

object Readers {

  /**
   * Readers are data structures that solve the following scenario.
   * 
   * Suppose we have a multi-layered application and a configuration (config
   * file) that feeds (stores data about) all these layers.
   *
   * - config file => initial data structure
   * - DB layer     (username, password, etc.)
   * - HTTP layer   (hostname, port, etc.)
   * - business logic layer
   *
   * The Reader data type will be able to feed the initial data structure
   * (built from the config file) to all the layers.
   */

  // initial data structure corresponding to config file
  case class Configuration(dbUsername:    String,
                           dbPassword:    String,
                           hostname:      String,
                           port:          Int,
                           nThreads:      Int,
                           emailReplyTo:  String)

  // database layer
  case class DbConnection(username: String, password: String) {
    // in real life: select * from db table and return order status
    def getOrderStatus(orderId: Long): String = "dispatched"
    def getLastOrderId(username: String): Long = 542643 // select max(orderID) from table where username = username
  }

  // http layer
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server in real life
  }

  // bootstrap the application: read following data from a config file
  val config = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "daniel@rockthejvm.com")

  // We can now insert a reader to inform the creation of a db connection and an
  // http service based on this Configuration object. A reader is a Cats
  // data type that implements this pattern.

  import cats.data.Reader
  // Configuration is the input and the DbConnection is the output
  // The Reader constructor takes a function from the input to the output,
  // which means the Reader is just a wrapper over this function
  val dbReader: Reader[Configuration, DbConnection] =
    Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))

  /*
    Inject a configuration object into this reader instance.
    Derive a db connection from this Reader as follows. When we pass the
    config object, the dbReader will run the function we provided in the
    constructor and return the db connection.
  */
  val dbConn = dbReader.run(config)

  /*
    A reader has interesting properties.
    If we call map on Reader[I, O], then the output type O can be transformed
    into something else. Example of transforming the output type from DbConnection
    to String:
   */
  val danielsOrderStatusReader: Reader[Configuration, String] =
    dbReader map (dbconn => dbconn.getOrderStatus(55))

 /*
  So this would first run the function
    conf => DbConnection(conf.dbUsername, conf.dbPassword)
  followed by
    dbconn => dbconn.getOrderStatus(55)

    To find Daniel's order status based on the config file, we can run
  */
  val danielsOrderStatus: String = danielsOrderStatusReader.run(config)

  /*
    This means we can pass something that we can pass the configuration --
    something that we create all the way at the bootstrap stage and obtain
    something very particular in the middle of the application. Readers
    are useful for this kind of situation.

    We create a data structure (config in the above example) at the
    beginning and we can derive all kinds of useful information from it
    at later stages. The pattern is as follows.

    Pattern
    - Create the initial data structure
    - Create a reader which specifies how that data structure will be manipulated later
    - Map and flatMap the reader to produce derived information
    - When we need the final piece of information, we call run on the reader
      with the initial data structure
  */
  //---------------------------------------------------------------------------
  // Example 2

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] =
      dbReader.map(_.getLastOrderId(username)) // type is Reader[Configuration, Long]
        .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // exactly the same as above, but written with for comprehension
    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader map (_.getLastOrderId(username))
      orderStatus <- dbReader map (_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor run config
    // usersLastOrderIdReader.run(config) // alternative implementation
  }

  //---------------------------------------------------------------------------
  // Exercise
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String =
      s"From: $emailReplyTo, To: $address >>> $contents"
  }

  // TODO 1
  // fetch the status of their last order
  // email them with the Email service: "Your last order has the status: (status)"
  def emailUser(username: String, userEmail: String): String = {
    val emailServiceReader: Reader[Configuration, EmailService] =
      Reader(conf => EmailService(conf.emailReplyTo))

    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader map (_.getLastOrderId(username))
      orderStatus <- dbReader map (_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your last order has the status: $orderStatus")

    // Run the email reader to send the email
    emailReader run config
  }

  // TODO 2. What programming pattern do Readers remind you of?
  // Dependency injection, as the config is injected right at the end in the run method.

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("daniel", "daniel@rtjvm.com"))
  }
}
