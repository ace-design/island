package eu.ace_design.island.game

import eu.ace_design.island.game.actions._
import eu.ace_design.island.map.resources.{ManufacturedResource, PrimaryResource, Resource}
import eu.ace_design.island.util.{Logger, LogSilos}
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.json.{JSONArray, JSONObject}
import scala.util.Random


/**
 * An action is used to model an action taken by a player's bot.
 */
trait Action extends Logger {

  override val silo = LogSilos.GAME_ENGINE

  /**
   * Apply the action to a game, using the information stored in the board.
   * @param board the game board
   * @param game the game used to store the contents of the exploration
   * @return an updated version of the game that include the result of the action, and the result
   */
  final def apply(board: GameBoard, game: Game): (Game, Result) = {
    val result: Result = build(board, game)
    try { game updatedBy result } catch { case e: Exception => (game, ExceptionResult(e)) }
  }

  /**
   * Build the result associated to a given action
   * @param board the game board
   * @param game the game used to store the contents of the exploration
   * @return an instance of Result, with the eight cost
   */
   private def build(board: GameBoard, game: Game): Result = {
    val overhead  = Random.nextInt(4) + 1                 // overhead \in [1,5]
    val variation = 1 + (Random.nextDouble() / 2) - 0.25 // variation \in [0.75, 1.25]
    val result = buildResult(board, game)  // will throw an exception if the action cannot be performed
    val rawCost   = computeCost(board, game)
    val cost = 1.0 + ( (overhead + rawCost) * variation)
    val realCost =  Math.max(2.0,cost).ceil.toInt
    result withCost realCost
  }

  /**
   * This method computes the cost of execution for the action
   * @param board the game board
   * @param game the game used to store the contents of the exploration
   * @return an integer value
   */
  def computeCost(board: GameBoard, game: Game): Double

  /**
   * This method computes the result of the execution for this action
   * @param board the game board
   * @param game the game used to store the contents of the exploration
   * @return an dedicated result for this specific action
   */
  def buildResult(board: GameBoard, game: Game): Result

}


trait ActionWithDirection extends Action {
  val direction: Directions.Direction

  protected def nextLoc(g: Game): (Int, Int) = Directions.move(g.crew.location.get._1, g.crew.location.get._2, direction)
}

/**
 * the ActionParser is used to parse JSON-based representation of actions into Actions
 */
object ActionParser {

  /**
   * Functional representation used to parse a json string into an action
   * @param data the json string modeling the action taken by the player
   * @return the associated instance of action, or an IllegalArgumentException
   */
  def apply(data: String): Action = try {
    val json  =  new JSONObject(data)
    json.getString("action") match {
      case Actions.LAND      => land(json.getJSONObject("parameters"))
      case Actions.EXPLORE   => Explore()
      case Actions.MOVE_TO   => moveTo(json.getJSONObject("parameters"))
      case Actions.SCOUT     => scout(json.getJSONObject("parameters"))
      case Actions.EXPLOIT   => exploit(json.getJSONObject("parameters"))
      case Actions.STOP      => Stop()
      case Actions.GLIMPSE   => glimpse(json.getJSONObject("parameters"))
      case Actions.TRANSFORM => transform(json.getJSONObject("parameters"))
      case Actions.FLY       => Fly()
    }
  } catch {
    case e: Exception => throw new IllegalArgumentException(s"Invalid JSON input : $e \ndata: $data")
  }


  def letter2Direction(params: JSONObject): Directions.Value = {
    params.getString("direction") match {
      case "N" => Directions.NORTH
      case "S" => Directions.SOUTH
      case "E" => Directions.EAST
      case "W" => Directions.WEST
    }
  }

  def string2Resource(params: JSONObject): Resource = Resources.bindings(params.getString("resource"))


  /**********************************************
   * Private helpers to build 'complex' actions *
   **********************************************/

  private def land(params: JSONObject) = Land(creek = params.getString("creek"), people = params.getInt("people"))

  private def moveTo(params: JSONObject) = MoveTo(direction = letter2Direction(params))

  private def scout(params: JSONObject) = Scout(direction = letter2Direction(params))

  private def exploit(params: JSONObject) = Exploit(resource = string2Resource(params).asInstanceOf[PrimaryResource])

  private def glimpse(params: JSONObject) = Glimpse(range = params.getInt("range"), direction = letter2Direction(params))

  private def transform(params: JSONObject) = {
    import scala.collection.JavaConversions._
    val materials =
      (params.keys map  { k => Resources.bindings(k).asInstanceOf[PrimaryResource] -> params.getInt(k) }).toMap
    Transform(materials)
  }
}