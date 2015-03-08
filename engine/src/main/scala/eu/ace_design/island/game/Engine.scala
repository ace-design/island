package eu.ace_design.island.game

import eu.ace_design.island.bot.IExplorerRaid
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.util.LogSilos.Kind
import eu.ace_design.island.util.{LogSilos, Logger, Timeout}
import scala.collection.mutable.ListBuffer
import scala.util.Random
import org.json.{JSONArray, JSONObject}

/**
 * The engine is used to play a party
 **/
class Engine(val board: GameBoard, val game: Game, rand: Random = new Random()) extends Logger with Timeout {

  override protected val silo: Kind = LogSilos.GAME_ENGINE

  final val DEFAULT_TIMEOUT_VALUE = 2

  // run a game using a given explorer. Use mutable state for the events (=> UGLY)
  def run(explorer: IExplorerRaid): (Seq[ExplorationEvent], Game) = {
    info(s"Starting game for ${explorer.getClass.getCanonicalName}")
    val events = ListBuffer.empty[ExplorationEvent]

    // Setting up context
    try {
      val context = buildInitializationContext()
      events += ExplorationEvent(Actors.Engine, context, method = "initialize")
      info("Initializing context [explorer.initializeContext(...)]")
      timeout(DEFAULT_TIMEOUT_VALUE) { explorer.initialize(context.toString) }
    } catch {
      case e: Exception => return (events += ExplorationEvent(Actors.Explorer, e, "initialize"), game.flaggedAsKO)
    }

    val gameAfter = play(explorer, events, game)

    info("Game is over")
    // returning the events
    (events, gameAfter)
  }

  // play an action took by explorer, using events to store the log of the exploration
  // the method is recursive
  def play(explorer: IExplorerRaid, events: ListBuffer[ExplorationEvent], g: Game): Game = {
    // ask player for decision:
    val action = try {
      info("Asking for user's decision [explorer.takeDecision()]")
      val str = timeout(DEFAULT_TIMEOUT_VALUE) { explorer.takeDecision() }
      events += ExplorationEvent(Actors.Explorer, new JSONObject(str))
      ActionParser(str)
    } catch {
      case e: Exception => events += ExplorationEvent(Actors.Explorer, e, "takeDecision"); return g.flaggedAsKO
    }

    // Handling the action from the engine point of view
    val result = try {
      info("Applying user's decision to the board")
      val (after, result) = action(board, g)
      events += ExplorationEvent(Actors.Engine, result.toJson)
      try {
        info("Acknowledging results [explorer.acknowledgeResults(...)]")
        timeout(DEFAULT_TIMEOUT_VALUE) { explorer.acknowledgeResults(result.toJson.toString) }
      } catch {
        case e: Exception => {
          events += ExplorationEvent(Actors.Explorer, e, "acknowledgeResults")
          return after.flaggedAsKO
        }
      }
      result.shouldStop match {
        case false => play(explorer, events, after) // recursive call for game continuation
        case true  => result match {  // end of the game
          case excResult: ExceptionResult => after.flaggedAsKO // as an error
          case _ => after // as a normal stop
        }
      }
    } catch {
      case e: Exception => events += ExplorationEvent(Actors.Engine, e, "takeDecision"); g.flaggedAsKO
    }
    result
  }

  def buildInitializationContext(): JSONObject = {
    val creeks = board.findPOIsByType(Creek(null, null)) map { case (k,v) => v }
    val creek = creeks.toSeq(rand.nextInt(creeks.size)).identifier
    val ctx = new JSONObject()
    ctx.put("creek", creek); ctx.put("budget", 600); ctx.put("men", 50)
    val objs = new JSONArray()
    objs.put(new JSONObject("""{ "resource": "WOOD", "amount": 600 }"""))
    ctx.put("objective",objs)
    ctx
  }


}


object Actors extends Enumeration {
  type Actor = Value
  val Explorer, Engine = Value
}

object ExplorationEvent {
  final val DEFAULT_METHOD = "takeDecision"

  def apply(actor: Actors.Actor, data: JSONObject, method: String = DEFAULT_METHOD) =
    new ExplorationEvent(System.currentTimeMillis(), actor, data, method)

  def apply(actor: Actors.Actor, e: Exception, method: String) = {
    val error = new JSONObject()
    error.append("exception", e.getClass.getSimpleName)
    error.append("message", e.getMessage)
    error.append("stacktrace", e.getStackTrace)
    new ExplorationEvent(System.currentTimeMillis(), actor, error, method)
  }

}
class ExplorationEvent private(val timestamp: Long, val actor: Actors.Actor, data: JSONObject, method: String) {

  def toJson: String = {
    val result = new JSONObject()
    result.put("data", data)      ; result.put("part", actor)
    result.put("time", timestamp) ; result.put("meth", method)
    result.toString(2)
  }

  override def toString = toJson

}