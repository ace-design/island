package eu.ace_design.island.game

import eu.ace_design.island.bot.IExplorerRaid
import eu.ace_design.island.util.Timeout
import scala.collection.mutable.ListBuffer
import scala.util.Random
import org.json.JSONObject

/**
 * The engine is used to play a party
 **/
class Engine(val board: GameBoard, val game: Game, rand: Random = new Random()) extends Timeout {

  final val DEFAULT_TIMEOUT_VALUE = 2

  // run a game using a given explorer. Use mutable state for the events (=> UGLY)
  def run(explorer: IExplorerRaid): Seq[ExplorationEvent] = {

    val events = ListBuffer.empty[ExplorationEvent]

    // Setting up context
    try {  propagateContext(explorer, events) } catch {
      case e: Exception => return events += ExplorationEvent(Actors.Explorer, e, "initialize")
    }

    play(explorer, events, game)

    // returning the events
    events
  }

  def play(explorer: IExplorerRaid, events: ListBuffer[ExplorationEvent], g: Game) {
    // ask player for decision:
    val action = try {
      val str = timeout(DEFAULT_TIMEOUT_VALUE) { explorer.takeDecision() }
      events += ExplorationEvent(Actors.Explorer, new JSONObject(str))
      ActionParser(str)
    } catch {
      case e: Exception => events += ExplorationEvent(Actors.Explorer, e, "takeDecision"); return
    }

    // Handling the action from the engine point of view
    try {
      val (after, result) = action(board, g)
      events += ExplorationEvent(Actors.Engine, result.toJson)
      result.ok match {
        case false =>
        case true  => play(explorer, events, after)
      }
    } catch {
      case e: Exception => events += ExplorationEvent(Actors.Engine, e, "takeDecision")
    }
  }

  def propagateContext(e: IExplorerRaid, events: ListBuffer[ExplorationEvent]) = {
    val context = buildInitializationContext()
    events += ExplorationEvent(Actors.Engine, context, method = "initialize")
    timeout(DEFAULT_TIMEOUT_VALUE) { e.initialize(context.toString) }
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