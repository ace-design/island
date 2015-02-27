package eu.ace_design.island.util



/**
 * This trait defines a timeout structure to be used to timebox the execution of a given code block
 **/
trait Timeout {

  import scala.concurrent._
  import duration._
  import ExecutionContext.Implicits.global

  def timeout[T](clock: Int)(block: => T): T = {
    val future = Future { block }
    Await.result(future, clock.milliseconds)
  }

}
