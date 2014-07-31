package eu.ace_design.island.util



/**
 * This trait is used to introduce logging mechanisms in the Island classes
 * @author mosser
 */
trait Logger {
  import LogSilos._

  import org.apache.logging.log4j.LogManager
  import org.apache.logging.log4j.{Logger => Log4JLogger}

  protected val thisLogger: Log4JLogger = LogManager.getLogger(this.getClass.getCanonicalName)
  System.setProperty("Log4jContextSelector","org.apache.logging.log4j.core.async.AsyncLoggerContextSelector")

  val silo: Kind

  def trace(msg: String, s: Kind = silo) { Loggers(s) trace msg }
  def debug(msg: String, s: Kind = silo) { Loggers(s) debug msg }
  def info (msg: String, s: Kind = silo) { Loggers(s) info msg  }
  def warn (msg: String, s: Kind = silo) { Loggers(s) warn msg  }
  def error(msg: String, s: Kind = silo) { Loggers(s) error msg }
  def fatal(msg: String, s: Kind = silo) { Loggers(s) fatal msg }
}


object LogSilos extends Enumeration {
  type Kind = Value
  val ROOT, MESH_GEN, MAP_GEN, VIEWER = Value
}

object Loggers {
  import LogSilos._
  import org.apache.logging.log4j.LogManager
  import org.apache.logging.log4j.{Logger => Log4JLogger}

  def apply(s: LogSilos.Kind): Log4JLogger = _others.getOrElse(s,_root)


  private val PREFIX = this.getClass.getCanonicalName

  private val _root = LogManager.getLogger(PREFIX)

  private val _others: Map[Kind,Log4JLogger] = Map(
    ROOT     -> _root,
    MESH_GEN -> LogManager.getLogger(s"${PREFIX}Mesh"),
    MAP_GEN  -> LogManager.getLogger(s"${PREFIX}Map"),
    VIEWER   -> LogManager.getLogger(s"${PREFIX}Viewer")
  )
}


