package eu.ace_design.island.game

import eu.ace_design.island.bot.IExplorerRaid
import eu.ace_design.island.util.LogSilos.Kind
import eu.ace_design.island.util.{LogSilos, Logger, Timeout}
import scala.collection.mutable.ListBuffer
import scala.util.Random
import org.json.JSONObject

/**
 * The engine is used to play a party
 **/
class Engine(val board: GameBoard, val game: Game, rand: Random = new Random()) extends Logger with Timeout {

  override protected val silo: Kind = LogSilos.GAME_ENGINE

  final val DEFAULT_TIMEOUT_VALUE = 2

  // run a game using a given explorer. Use mutable state for the events (=> UGLY)
  def run(explorer: IExplorerRaid): Seq[ExplorationEvent] = {
    info(s"Starting game for ${explorer.getClass.getCanonicalName}")
    val events = ListBuffer.empty[ExplorationEvent]

    // Setting up context
    try {
      val context = buildInitializationContext()
      events += ExplorationEvent(Actors.Engine, context, method = "initialize")
      info("Initializing context [explorer.initializeContext(...)]")
      timeout(DEFAULT_TIMEOUT_VALUE) { explorer.initialize(context.toString) }
    } catch {
      case e: Exception => return events += ExplorationEvent(Actors.Explorer, e, "initialize")
    }

    play(explorer, events, game)

    info("Game is over")
    // returning the events
    events
  }

  // play an action took by explorer, using events to store the log of the exploration
  // the method is recursive
  def play(explorer: IExplorerRaid, events: ListBuffer[ExplorationEvent], g: Game) {
    // ask player for decision:
    val action = try {
      info("Asking for user's decision [explorer.takeDecision()]")
      val str = timeout(DEFAULT_TIMEOUT_VALUE) { explorer.takeDecision() }
      events += ExplorationEvent(Actors.Explorer, new JSONObject(str))
      ActionParser(str)
    } catch {
      case e: Exception => events += ExplorationEvent(Actors.Explorer, e, "takeDecision"); return
    }

    // Handling the action from the engine point of view
    try {
      info("Applying user's decision to the board")
      val (after, result) = action(board, g)
      events += ExplorationEvent(Actors.Engine, result.toJson)
      try {
        info("Acknowledging results [explorer.acknowledgeResults(...)]")
        timeout(DEFAULT_TIMEOUT_VALUE) { explorer.acknowledgeResults(result.toJson.toString) }
      } catch {
        case e: Exception => events += ExplorationEvent(Actors.Explorer, e, "acknowledgeResults"); return
      }
      result.shouldStop match {
        case false => play(explorer, events, after) // recursive call for game continuation
        case true  =>                               // end of the game
      }
    } catch {
      case e: Exception => events += ExplorationEvent(Actors.Engine, e, "takeDecision")
    }
  }

  def propagateContext(e: IExplorerRaid, events: ListBuffer[ExplorationEvent]) = {

  }

  def buildInitializationContext(): JSONObject = {
    // TODO identify the right creek identifier and build a right JSON object
    val data =
      """{ "creek": "creek_id", "budget": 600, "men": 50,
        |  "objective": [ { "resource": "WOOD", "amount": 600 } ] } """.stripMargin
    new JSONObject(data)
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