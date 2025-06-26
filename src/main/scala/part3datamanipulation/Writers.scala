package part3datamanipulation

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer

  // 1. Define them at the start
  // a writer takes two data types: a log type and a value type
  val aWriter: Writer[List[String], Int] = Writer(List("started something"), 45)

  /*
    A Writer is a wrapper over a valuable value, in this case an Int. As we transform
    this data, we can also keep track of additional information such as the sequence
    of modifications, some sort of logs, that we might want to dump into a file
    at the end. Cats provides APIs to transform both the desired value and the
    logs value.
   */
  // 2. Manipulate them with pure FP throughout the application
  val anIncreasedWriter = aWriter map (_ + 1) // value increases; logs stay the same
  val aLogsWriter = aWriter mapWritten(_ :+ "found something interesting") // log changes; value stays the same

  // Two ways of modifying both log and value:
  val aWriterWithBoth = aWriter bimap (_ :+ "found something interesting", _ + 1)

  // With mapBoth, the value and the log and the value can influence each other
  val aWriterWithBoth_v2 = aWriter mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }
  /*
    Writers are useful because we can define them once at the start of the application
    and then manipulate them in a purely functional way as they pass through the
    application. At the end we can dump the value or the logs using methods from
    the Writer api.
   */

  //---------
  // Other manipulations: flatMap
  // In the following example, the values are combined with the for comprehension's
  // yield, while the logs are combined by the combine function of a semigroup.
  // (For logs we usually use a Vector rather than a List because Vector
  // is faster for appending and combining.)

  // The following import was needed in older versions of Scala and/or Cats
  // import cats.instances.vector._ // imports a Semigroup[Vector]

  val writerA = Writer(Vector("Log A1", "Log A2"), 45)
  val writerB = Writer(Vector("Log B1"), 10)

  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb
  // println(compositeWriter.run) // (Vector(Log A1, Log A2, Log B1),55)

  //---------
  // reset the logs
  // import cats.instances.list._ // Was needed for older versions of Scala/Cats to fetch an implicit Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset   // clear the logs, keep the value
  // println(anEmptyWriter) // WriterT((List(),45))

  //---------
  // 3. Dump either the value or the logs
  val desiredValue = aWriter.value  // extract the value
  val logs = aWriter.written        // extract the logs
  val (log, value) = aWriter.run    // extract both using tuple pattern matching

  //---------------------------------------------------------------------------
  // TODO 1: Rewrite the following function to "print" things with writers
  // function is stack-recursive
  def countAndSay(n: Int): Unit =
    if (n <= 0) println("Starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }

  // Following solution uses flatMap, although bimap or mapBoth will also work.
  // It is stack-recursive. To make it tail recursive, add writer as an accumulator
  def countAndLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0)
      Writer(Vector("Starting!"), 0)
    else
      countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))

  // Benefit #1: We use pure FP. No side-effects such as printing to the console.
  // It's a purely functional way of dealing with logs or tracking changes.

  //---------------------------------------------------------------------------
  // TODO 2: Rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum (${n-1}) = $lowerSum")
      lowerSum + n
    }
  }

  // Solution
  // In the for comprehension, the logs of the Writers are composed with a
  // Semigroup's combine function, whereas the value we are passing, n, does not matter.
  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _         <- Writer(Vector(s"Now at $n"), n) // value does not matter but log does.
      lowerSum  <- sumWithLogs(n - 1) // this is where the desirable value comes from.
      _         <- Writer(Vector(s"Computed sum (${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n
  }

  // Benefit #2: Writers can keep logs separate on multiple threads

  //---------------------------------------------------------------------------
  val e: ExecutorService = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(e)

  def main(args: Array[String]): Unit = {
    // println(compositeWriter.run) // (Vector(Log A1, Log A2, Log B1),55)
    // println(anEmptyWriter) // WriterT((List(),45))

    // Exercise 1
    // println(countAndSay(10))
    // println(countAndLog(10).run) // (Vector(Starting!, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),10)
    // countAndLog(10).written.foreach(println)

    // Exercise 2
    // println(naiveSum(10))

    // If we run it concurrently on two Futures, the logs get interspersed and we can't tell
    // which log came from which thread.
    // Future(naiveSum(100)).foreach(println)
    // Future(naiveSum(100)).foreach(println)

    // sumWithLogs(10).written.foreach(println) // output is exactly the same as that of naiveSum(10)
    // Whereas the logs don't get interspersed if we run them concurrently on two Futures using Writers
    val sumFuture1 = Future(sumWithLogs(100))
    val sumFuture2 = Future(sumWithLogs(100))
    val logs1 = sumFuture1.map(_.written).foreach(println)
    val logs2 = sumFuture2.map(_.written).foreach(println)
  }
}
