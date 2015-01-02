package eu.ace_design.island.util

/**
 * This trait is used to introduce logging mechanisms in the Island classes. A model element wanting to log
 * things must mix this trait.
 *
 * Runtime configuration is done in the 'resources/log4j2.xml' file.  To subscribe to a given log silo, one simply need
 * to create a Logger in the config file, referencing the Log4J2 logger used by this silo. For example, to listen to
 * the logs produced in the MESH_GEN silo, one should create a logger using the following name:
 * eu.ace_design.Island/Mesh. The binding between silos and concrete loggers name is located in the Loggers object.
 *
 * This design decision was taken to ease the logging process. While generating a map, one is actually interested in
 * the actions done during a specific phase, and thus the classical logging mechanism associated to the class was
 * tedious. To log the map building, one had to (consistently) declare up to 5 loggers! If some information need to
 * be logged as the class level, a particular logger called 'thisLogger' is provided by the trait, and can be used as
 * any classical Log4J2 logger.
 *
 */
trait Logger {
  import LogSilos._

  import org.apache.logging.log4j.LogManager
  import org.apache.logging.log4j.{Logger => Log4JLogger}

  // Necessary, without a runtime exception is thrown as no logger are available
  protected val thisLogger: Log4JLogger = LogManager.getLogger(this.getClass.getCanonicalName)
  System.setProperty("Log4jContextSelector","org.apache.logging.log4j.core.async.AsyncLoggerContextSelector")

  // A logger is connected to a given silo (to be configured in 'resources/log4j2.xml'). The silo is statically
  // chosen at compilation time, as this variable must be specified (override val) in the targeted class
  protected val silo: Kind

  // the name of the target class, used to generate useful message. Object ending $ (scala convention) are removed.
  val name = {
    val n = this.getClass.getSimpleName
    if ( n endsWith "$") n.substring(0,n.size-1) else n
  }

  /**
   ** Functions used to propagate the message to log to the good logger. By default, to the one associated to our silo.
   **/

  def trace(msg: => String, s: Kind = silo) { if (logger(s).isTraceEnabled) { logger(s) trace s"[$name] $msg" } }
  def debug(msg: => String, s: Kind = silo) { if (logger(s).isDebugEnabled) { logger(s) debug s"[$name] $msg" } }
  def info (msg: => String, s: Kind = silo) { if (logger(s).isInfoEnabled)  { logger(s) info  s"[$name] $msg" } }
  def warn (msg: => String, s: Kind = silo) { if (logger(s).isWarnEnabled)  { logger(s) warn  s"[$name] $msg" } }
  def error(msg: => String, s: Kind = silo) { if (logger(s).isErrorEnabled) { logger(s) error s"[$name] $msg" } }
  def fatal(msg: => String, s: Kind = silo) { if (logger(s).isFatalEnabled) { logger(s) fatal s"[$name] $msg" } }

  protected def logger(s: Kind) = Loggers(s)

}



/**
 * the different silos available in the application, defined as a static enumeration
 */
object LogSilos extends Enumeration {
  type Kind = Value
  val ROOT, MESH_GEN, MAP_GEN, VIEWER, TEST, BOARD_GEN = Value
}

/**
 * An indirection table to pickup the right logger when asked for (see propagation function in the Logger trait)
 */
object Loggers {
  import LogSilos._
  import org.apache.logging.log4j.LogManager
  import org.apache.logging.log4j.{Logger => Log4JLogger}

  def apply(s: LogSilos.Kind): Log4JLogger = _others.getOrElse(s,_root)


  private val PREFIX = "eu.ace_design.Island"

  private val _root = LogManager.getLogger(PREFIX)

  private val _others: Map[Kind,Log4JLogger] = Map(
    ROOT      -> _root,
    MESH_GEN  -> LogManager.getLogger(s"$PREFIX/Mesh"),
    MAP_GEN   -> LogManager.getLogger(s"$PREFIX/Map"),
    VIEWER    -> LogManager.getLogger(s"$PREFIX/Viewer"),
    TEST      -> LogManager.getLogger(s"$PREFIX/Test"),
    BOARD_GEN -> LogManager.getLogger(s"$PREFIX/Board")
  )
}


