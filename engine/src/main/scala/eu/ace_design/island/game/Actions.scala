package eu.ace_design.island.game

import eu.ace_design.island.map.resources.{Conditions, Soils, PrimaryResource, Resource}
import eu.ace_design.island.util.{Logger, LogSilos}
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.json.JSONObject
import scala.util.Random


/**
 * An action is used to model an action taken by a player's bot.
 */
sealed trait Action extends Logger {

  override val silo = LogSilos.GAME_ENGINE

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
    debug("  Stop:")
    val cost = game.boat match {
      case None => overhead * variation
      case Some((x,y)) => {
        val center = board.size / 2
        val distance = Math.sqrt(Math.pow(x-center,2) + Math.pow(y-center,2))
        val ratio = board.m.size.toFloat / board.size / 10
        debug(s"    distance: $distance / ratio: $ratio ")
        (overhead + distance * ratio) * variation
      }
    }
    info(s"  Stop: cost = $cost")
    EmptyResult(cost.ceil.toInt, shouldStop = true)
  }

}

/**
 * The Land action
 * @param creek
 * @param people
 */
case class Land(creek: String, people: Int) extends Action          {

  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = {
    debug("  Land:")
    if(people >= game.crew.complete)
      throw new IllegalArgumentException("At least one men must stay on board")
    val creeks = board.findPOIsByType(Creek(null, null))
    val (cost, loc) = creeks find  { case (loc,c) => c.identifier == creek } match {
      case None => throw new IllegalArgumentException(s"Unknown creek identifier [$creek]")
      case Some((loc,poi)) => {
        val origin = game.boat.getOrElse((board.size / 2, board.size / 2))
        val distance = Math.sqrt(Math.pow(loc._1- origin._1,2) + Math.pow(loc._2- origin._2,2))
        val ratio = board.m.size.toFloat / board.size / 10
        debug(s"    distance: $distance / ratio: $ratio ")
        ((overhead + distance * ratio) * variation, loc)
      }
    }
    info(s"  Land: cost = $cost")
    MovedBoatResult(cost.ceil.toInt, loc, people)
  }

}

/**
 * The move to action make the crew move to another tile on the map
 * @param direction
 */
case class MoveTo(direction: Directions.Direction) extends Action {

  import eu.ace_design.island.map.resources.PIXEL_FACTOR

  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = {
    debug("  MoveTo:")
    if(game.boat.isEmpty)
      throw new IllegalArgumentException("Cannot move without having landed before")
    val loc = game.crew.location.get  // cannot be None as game.boat is not empty
    val oldTile = board.at(loc._1, loc._2)
    val updatedLoc = Directions.move(loc._1, loc._2, direction)
    if(! board.tiles.keySet.contains(updatedLoc))
      throw new IllegalArgumentException("Congrats, you just fall out of the world limit.")

    val newTile = board.at(updatedLoc._1, updatedLoc._2)
    // Now we can move. Move is a function of men, altitude difference and biomes to cross.
    val cost = {
      val men = game.crew.landed
      val rise = Math.abs(oldTile.altitude - newTile.altitude) // altitude already stored as meters
      val run = board.tileUnit.toDouble * PIXEL_FACTOR // run to be transformed from pixel to meters
      val pitch = 100 * (rise / run) // http://en.wikipedia.org/wiki/Grade_(slope)
      val pitchFactor = Math.min(0.5, Math.abs(100 - pitch))
      val biomes = newTile.biomes
      val crossFactor = (0.0 /: biomes) { (acc, value) => acc + (value._1.crossFactor * value._2)} / 100
      val factor = (pitchFactor + 2 * crossFactor) / 3
      debug(s"    men: $men / pitch: $pitchFactor / cross: $crossFactor")
      debug(s"     ==>> factor: $factor")
      (overhead + (2.0 * men * factor)) * variation
    }
    info(s"  MoveTo: cost = $cost")
    MovedCrewResult(cost = cost.ceil.toInt, loc = updatedLoc)
  }
}


/**
 * the scout action returns information about neighbours tiles
 * @param direction
 */
case class Scout(direction: Directions.Direction) extends Action {

  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = {
    if(game.boat.isEmpty)
      throw new IllegalArgumentException("Cannot move without having landed before")
    val loc = game.crew.location.get  // cannot be None as game.boat is not empty
    val oldTile = board.at(loc._1, loc._2)
    val updatedLoc = Directions.move(loc._1, loc._2, direction)
    if(! board.tiles.keySet.contains(updatedLoc))
      return ScoutResult(cost = overhead, resources = Set(), altitude = 0, unreachable = true)
    val newTile = board.at(updatedLoc._1, updatedLoc._2)

    val res = newTile.stock map { _.resource.asInstanceOf[Resource] }
    val delta = oldTile.altitude - newTile.altitude
    val cost = {
      val men = game.crew.landed
      val factor = (0.0 /: newTile.biomes) { (acc, value) => acc + (value._1.crossFactor * value._2)} / 100
      (overhead + (men * factor)) * variation
    }
    ScoutResult(cost = cost.ceil.toInt, resources = res, altitude = delta.ceil.toInt)
  }
}


// { "action": "explore" }
case class Explore() extends Action {
  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = {
    if(game.boat.isEmpty)
      throw new IllegalArgumentException("Cannot move without having landed before")
    val loc = game.crew.location.get  // cannot be None as game.boat is not empty
    val tile = board.at(loc._1, loc._2)
    val resources = tile.stock map { stock =>
      val max = stock.resource.perHectare.toDouble / (board.tileUnit * board.tileUnit * 100)
      val alreadyExtracted = game.harvested(stock.resource,loc)
      val avail = stock.amount - alreadyExtracted
      val amount = avail match {
        case a if a <= max / 3      => ResourceLevels.LOW
        case a if a >= 2 * max / 3  => ResourceLevels.HIGH
        case _ => ResourceLevels.MEDIUM
      }
      val condition = stock.extraction match {
        case e if e <= 0.6 => Conditions.HARSH
        case e if e >= 1.3 => Conditions.EASY
        case _ => Conditions.FAIR
      }
      ResourceExploration(resource = stock.resource, amount = amount, condition = condition)
    }
    val pois = board.pois.getOrElse((loc._1, loc._2), Seq()).toSet
    val factor = (0.0 /: tile.biomes) { (acc, value) => acc + (value._1.crossFactor * value._2)} / 100
    val men = game.crew.landed
    val cost =  (overhead + (3 * men * factor)) * variation
    ExploreResult(cost = cost.ceil.toInt, resources = resources, pois = pois)
  }
}


// { "action": "exploit", "parameters": { "resource": "..." } }
case class Exploit(resource: PrimaryResource) extends Action {
  override protected def build(board: GameBoard, game: Game, overhead: Int): Result = {
    if(game.boat.isEmpty)
      throw new IllegalArgumentException("Cannot move without having landed before")
    val loc = game.crew.location.get  // cannot be None as game.boat is not empty
    val tile = board.at(loc._1, loc._2)
    val extracted = tile.stock.find(s => s.resource == resource) match {
      case None => throw new IllegalArgumentException(s"No resource [$resource] available on the current tile")
      case Some(stock) => {
        val alreadyExtracted = game.harvested(resource,loc)
        val avail = stock.amount - alreadyExtracted
        // do the extraction
        val theoretical = (game.crew.landed * 0.5 * stock.extraction * avail * variation).ceil.toInt
        val amount = Math.min(avail, theoretical)
        amount
      }
    }
    val boat = game.boat.get
    val distance = Math.sqrt(Math.pow(loc._1 -boat._1,2)+Math.pow(loc._2 -boat._2,2))
    val cost = overhead + ( 2 * distance * game.crew.landed * extracted / 10.0)
    ExploitResult(cost = cost.ceil.toInt, amount = extracted, r = resource)
  }
}


// { "action": "glimpse" }
case class Glimpse() extends Action {
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