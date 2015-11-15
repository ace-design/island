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

  final val DEFAULT_TIMEOUT_VALUE = 2000 // The player can take up to 2s to answer to queries

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
      val (after, r) = action(board, g)
      events += ExplorationEvent(Actors.Engine, r.toJson)
      try {
        info("Acknowledging results [explorer.acknowledgeResults(...)]")
        timeout(DEFAULT_TIMEOUT_VALUE) { explorer.acknowledgeResults(r.toJson.toString) }
      } catch {
        case e: Exception => {
          events += ExplorationEvent(Actors.Explorer, e, "acknowledgeResults")
          return after.flaggedAsKO
        }
      }
      r.shouldStop match {
        case false => play(explorer, events, after) // recursive call for game continuation
        case true  => r match {  // end of the game
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
    val ctx = new JSONObject()
    ctx.put("budget", game.budget.initial)
    ctx.put("men", game.crew.complete)
    val objs = new JSONArray()
    (game.objectives :\ objs) { (elem, acc) => {
      val deal = new JSONObject(); deal.put("resource", elem._1.name); deal.put("amount", elem._2)
      acc.put(deal)
    }}
    ctx.put("contracts",objs)
    ctx.put("heading", game.plane.get.heading.toString.substring(0,1))
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