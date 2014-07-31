package eu.ace_design.island.util

/**
 * This trait is used to introduce logging mechanisms in the Island classes
 * @author mosser
 */
trait Logger {

  import org.apache.logging.log4j.LogManager
  import org.apache.logging.log4j.{Logger => Log4JLogger}

  protected val logger: Log4JLogger = LogManager.getLogger(this.getClass.getCanonicalName)

  System.setProperty("Log4jContextSelector","org.apache.logging.log4j.core.async.AsyncLoggerContextSelector")

}
