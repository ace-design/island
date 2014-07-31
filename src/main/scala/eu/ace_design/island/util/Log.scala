package eu.ace_design.island.util

/**
 * This trait is used to introduce logging mechanisms in the Island classes
 * @author mosser
 */
trait Log {

  import org.apache.logging.log4j.LogManager
  import org.apache.logging.log4j.Logger

  protected val logger: Logger = LogManager.getLogger(this.getClass.getCanonicalName)

  System.setProperty("Log4jContextSelector","org.apache.logging.log4j.core.async.AsyncLoggerContextSelector")

}
