package eu.ace_design.island.game

import eu.ace_design.island.map.resources.{PrimaryResource, Resource}
import eu.ace_design.island.stdlib.Resources
import org.json.JSONObject

/**
 * An action is used to model an action taken by a player's bot.
 */
trait Action {
  def apply(board: GameBoard, game: Game): (Game, Result)
}

// { "action": "stop" }
case class Stop() extends Action {
  override def apply(board: GameBoard, game: Game): (Game, Result) = {
    ???
  }
}

// { "action": "explore" }
case class Explore() extends Action {
  override def apply(board: GameBoard, game: Game): (Game, Result) = ???
}

// { "action": "land", "parameters": {"deck": "...", "people": n } }
case class Land(deck: String, people: Int) extends Action          {
  override def apply(board: GameBoard, game: Game): (Game, Result) = ???
}

// { "action": "move_to", "parameters": { "direction": "..." } }
case class MoveTo(direction: Directions.Direction) extends Action {
  override def apply(board: GameBoard, game: Game): (Game, Result) = ???
}

// { "action": "scout", "parameters": { "direction": "..." } }
case class Scout(direction: Directions.Direction) extends Action {
  override def apply(board: GameBoard, game: Game): (Game, Result) = ???
}

// { "action": "exploit", "parameters": { "resource": "..." } }
case class Exploit(resource: PrimaryResource) extends Action {
  override def apply(board: GameBoard, game: Game): (Game, Result) = ???
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

  private def land(params: JSONObject) = Land(deck = params.getString("deck"), people = params.getInt("people"))

  private def moveTo(params: JSONObject) = MoveTo(direction = letter2Direction(params))

  private def scout(params: JSONObject) = Scout(direction = letter2Direction(params))

  private def exploit(params: JSONObject) = Exploit(resource = string2Resource(params).asInstanceOf[PrimaryResource])

}