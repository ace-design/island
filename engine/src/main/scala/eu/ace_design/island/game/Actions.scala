package eu.ace_design.island.game

import eu.ace_design.island.map.resources.{PrimaryResource, Resource}
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.json.JSONObject
import scala.util.Random

/**
 * An action is used to model an action taken by a player's bot.
 */
sealed trait Action {

  // Maximal value for the action overhead
  val maxOverhead: Int = 10

  // The variation factor used by the action (\in [0.75, 1.25])
  final protected val variation: Double = 1 + (Random.nextDouble() / 2 - 0.25)

  // Apply the action to a given game, using the  game board representing the island
  final def apply(board: GameBoard, game: Game): (Game, Result) = {
    val overhead = Random.nextInt(maxOverhead) + 1
    val result: Result = build(board, game, overhead)
    try { game updatedBy result } catch { case e: Exception => (game, ExceptionResult(e)) }
  }

  // Method to be implemented by each action to build the associated result
  protected def build(board: GameBoard, game: Game, overhead: Int): Result

}

/**
 * The Stop action is used to exit the Island and stop the game. It produces an EmptyResult
 */
case class Stop() extends Action {

  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = {
    val cost = game.boat match {
      case None => overhead * variation
      case Some((x,y)) => {
        val center = board.size / 2
        val distance = Math.sqrt(Math.pow(x-center,2) + Math.pow(y-center,2))
        val ratio = board.m.size.toFloat / board.size / 10
        (overhead + distance * ratio) * variation
      }
    }
    EmptyResult(2 + cost.ceil.toInt, shouldStop = true)
  }

}

/**
 * The Land action
 * @param creek
 * @param people
 */
case class Land(creek: String, people: Int) extends Action          {

  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = {
    if(people >= game.crew.complete)
      throw new IllegalArgumentException("At least one men must stay on board")
    val creeks = board.findPOIsByType(Creek(null, null))
    val (cost, loc) = creeks find  { case (loc,c) => c.identifier == creek } match {
      case None => throw new IllegalArgumentException(s"Unknown creek identifier [$creek]")
      case Some((loc,poi)) => {
        val origin = game.boat.getOrElse((board.size / 2, board.size / 2))
        val distance = Math.sqrt(Math.pow(loc._1- origin._1,2) + Math.pow(loc._2- origin._2,2))
        val ratio = board.m.size.toFloat / board.size / 10
        ((overhead + distance * ratio) * variation, loc)
      }
    }
    MovedBoatResult(2 + cost.ceil.toInt, loc, people)
  }

}

// { "action": "move_to", "parameters": { "direction": "..." } }
case class MoveTo(direction: Directions.Direction) extends Action {

  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = {
    if(game.boat.isEmpty)
      throw new IllegalArgumentException("Cannot move without having landed before")
    val loc = game.crew.location

    ???
  }
}


// { "action": "explore" }
case class Explore() extends Action {
  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = ???
}





// { "action": "scout", "parameters": { "direction": "..." } }
case class Scout(direction: Directions.Direction) extends Action {
  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = ???
}

// { "action": "exploit", "parameters": { "resource": "..." } }
case class Exploit(resource: PrimaryResource) extends Action {
  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = ???
}


/**
 * Keywords to be used in JSON actions
 */
object Actions {
  final val LAND    = "land"
  final val EXPLORE = "explore"
  final val MOVE_TO = "move_to"
  final val SCOUT   = "scout"
  final val EXPLOIT = "exploit"
  final val STOP    =  "stop"
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
      case Actions.LAND    => land(json.getJSONObject("parameters"))
      case Actions.EXPLORE => Explore()
      case Actions.MOVE_TO => moveTo(json.getJSONObject("parameters"))
      case Actions.SCOUT   => scout(json.getJSONObject("parameters"))
      case Actions.EXPLOIT => exploit(json.getJSONObject("parameters"))
      case Actions.STOP    => Stop()
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

}