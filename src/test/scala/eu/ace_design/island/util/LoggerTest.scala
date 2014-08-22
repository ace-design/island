package eu.ace_design.island.util

import org.specs2.mutable._
import org.apache.logging.log4j.{LogManager, Level}
import org.apache.logging.log4j.core.LoggerContext

class LoggerTest extends SpecificationWithJUnit with Logger {

  override val silo = LogSilos.TEST

  "LoggerTest Specifications".title

  // Level Hierarchy: TRACE < DEBUG < INFO < WARN < ERROR < FATAL < ALL

  "A logger" should {
    "not evaluate its argument if not needed" in new infoLevelContext {
      trace((1/0).toString) must not( throwAn[ArithmeticException] )
      debug((1/0).toString) must not( throwAn[ArithmeticException] )
      info( (1/0).toString) must      throwAn[ArithmeticException]
      warn( (1/0).toString) must      throwAn[ArithmeticException]
      error((1/0).toString) must      throwAn[ArithmeticException]
      fatal((1/0).toString) must      throwAn[ArithmeticException]
    }
  }
}

trait infoLevelContext extends BeforeAfter {
  val silo = LogSilos.TEST
  val iniLvl = Loggers(silo).getLevel

  def setLevel(lvl: Level): Unit = {
    val context = LogManager.getContext(false).asInstanceOf[LoggerContext]
    val config = context.getConfiguration.getLoggerConfig(Loggers(silo).getName)
    config.setLevel(lvl)
    context.updateLoggers()
  }

  override def before { setLevel(Level.INFO) }
  override def after  { setLevel(iniLvl)     }
}
