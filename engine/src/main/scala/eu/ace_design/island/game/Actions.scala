package eu.ace_design.island.game

import eu.ace_design.island.map.resources.{ManufacturedResource, PrimaryResource, Resource}
import eu.ace_design.island.util.{Logger, LogSilos}
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.json.{JSONArray, JSONObject}
import scala.util.Random


/**
 * An action is used to model an action taken by a player's bot.
 */
sealed trait Action extends Logger {

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
 * The Stop action is used to exit the Island and stop the game. It produces an EmptyResult
 */
case class Stop() extends Action {

  override def computeCost(board: GameBoard, game: Game): Double = game.distanceToPort match {
    case None => 0
    case Some(toPort) => (game.menRatio * game.distanceToBoat) + Math.sqrt(toPort)
  }

  override def buildResult(board: GameBoard, game: Game): Result = EmptyResult(shouldStop = true)
}

/**
 * The Land action
 * @param creek
 * @param people
 */
case class Land(creek: String, people: Int) extends Action          {
  require(people > 0, "Cannot land less than one man")
  override def computeCost(board: GameBoard, game: Game): Double = {
    val creekLocation = board.findPOIsByType(Creek(null, null)).find { case (loc,c) => c.identifier == creek }.get._1
    // TODO update
    val comingBack = game.menRatio * Game.movingCostModel(game.normalizeMen) * game.distanceToBoat
    val landing = people / Game.MEN_RATIO  * Game.movingCostModel(game.normalizeMen)
    val moving = Math.sqrt(game.distanceByBoat(creekLocation))
    moving + comingBack + landing
  }


  override def buildResult(board: GameBoard, game: Game): Result = {
    require(people < game.crew.complete, "At least one men must stay on board")
    val creekData = board.findPOIsByType(Creek(null, null)) find { case (loc,c) => c.identifier == creek }
    require(creekData.isDefined, s"Unknown creek identifier [$creek]")
    val location = creekData.get._1
    MovedBoatResult(loc = location, men = people)
  }

}

/**
 * The move to action make the crew move to another tile on the map
 * @param direction
 */
case class MoveTo(override val direction: Directions.Direction) extends ActionWithDirection {

  override def computeCost(board: GameBoard, game: Game): Double = {
    val movingRawFactor = 1 + Game.movingCostModel(game.normalizeMen) // [0,1] => [0,1]
    val current = game.crew.location.get; val next = nextLoc(game)
    val pitchFactor = board.pitchFactor(current, next)
    val biomeFactor = (board.biomeFactor(current) + board.biomeFactor(next)) / 2
    val factor = (2*movingRawFactor + pitchFactor + 2*biomeFactor) / 5
    val result = game.menRatio * factor
    result
  }

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot move without having landed before")
    val next = nextLoc(game)
    require(board.tiles.keySet.contains(next), "Congrats, you just fall out of the world limit")
    MovedCrewResult(loc = next)
  }
}

/**
 * the scout action returns information about neighbours tiles
 * @param direction
 */
case class Scout(direction: Directions.Direction) extends ActionWithDirection {

  override def computeCost(board: GameBoard, game: Game): Double = {
    val next = nextLoc(game)
    val factor = board.tiles.get(next) match {
      case None => 0.0
      case Some(_) => (board.biomeFactor(game.crew.location.get) + board.biomeFactor(next)) / 2
    }
    Math.sqrt((1 + Random.nextInt(3)) * factor)
  }

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot scout without having landed before")
    val next = nextLoc(game)
    board.tiles.get(next) match {
      case None => ScoutResult(resources = Set(), altitude = 0, unreachable = true)
      case Some(nextTile) => {
        val crewLoc = game.crew.location.get
        val current = board.at(crewLoc._1, crewLoc._2)
        ScoutResult(resources = nextTile.resources, altitude = current.diffAltitude(nextTile))
      }
    }
  }

}


case class Glimpse(range: Int, override val direction: Directions.Direction) extends ActionWithDirection {

  require(range > 0,  "Range for glimpse must be greater than zero")
  require(range <= 4, "Range for glimpse cannot exceed 4")

  override def computeCost(board: GameBoard, game: Game): Double = math.sqrt(range)

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot glimpse without having landed before")
    val start = game.crew.location.get
    val raw = ((1 to range) :\ Seq[JSONArray]()) { (idx, acc) => buildReport(moveTo(idx, start), idx, board) +: acc }
    val report = raw filterNot { _.length() == 0 }
    GlimpseResult(report = report, asked = range)
  }

  private def buildReport(position: (Int, Int), range: Int, board: GameBoard): JSONArray = {
    if (!board.tiles.contains(position))
      return new JSONArray()
    val tile = board.at(position._1, position._2)
    val contents = tile.biomes map { case (b,v) => new JSONArray().put(0,b.name).put(1,round(v)) }
    range match {
      case x if x <= 2 => (contents.toSeq.sortBy { _.getDouble(1) } :\ new JSONArray()) { (rep, acc) => acc.put(rep) }
      case 3 => (contents.map { _.getString(0) } :\ new JSONArray()) { (elem, acc) => acc.put(elem) }
      case 4 => new JSONArray().put(contents.toSeq.sortBy( _ .getDouble(1) ).last.getString(0))
    }
  }

  private def moveTo(idx: Int, acc: (Int, Int)): (Int, Int) = idx match {
    case 1 => acc
    case _ => moveTo(idx-1, Directions.move(acc._1, acc._2, direction))
  }

  private def round(v: Double) = (v * 100 * 100).round / 100.0


}



// { "action": "explore" }
case class Explore() extends Action {

  override def computeCost(board: GameBoard, game: Game): Double = {
    val biomeFactor = board.biomeFactor(game.crew.location.get)
    val factor = (biomeFactor + Game.movingCostModel(game.normalizeMen)) / 2.0
    (1 + Random.nextInt(5)) * factor
  }

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot explore without having landed before")
    val current = game.crew.location.get  // cannot be None as game.boat is not empty
    val tile = board.at(current._1, current._2)
    val pois = board.pois.getOrElse((current._1, current._2), Seq()).toSet
    val resources = tile.stock map { stock =>
      val (lvl, cond) = stock.explore(board, game.harvested(stock.resource, game.crew.location.get))
      ResourceExploration(stock.resource, lvl, cond)
    }
    ExploreResult(resources = resources, pois = pois)
  }
}

// { "action": "exploit", "parameters": { "resource": "..." } }
case class Exploit(resource: PrimaryResource) extends Action {

  override def computeCost(board: GameBoard, game: Game): Double = {
    val exploitationFactor = Game.exploitationCostModel(game.normalizeMen)
    (game.crew.landed * exploitationFactor * resource.difficulty) + Math.sqrt(game.distanceToBoat)
  }

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.boat.isDefined, "Cannot explore without having landed before")
    val current = game.crew.location.get  // cannot be None as game.boat is not empty
    val tile = board.at(current._1, current._2)
    require(tile.stock.exists(s => s.resource == resource), "No resource [$resource] available on the current tile")
    val stock = tile.stock.find(s => s.resource == resource).get
    val alreadyExtracted = game.harvested(resource,current)
    val avail = stock.amount - alreadyExtracted
    val theoretical = game.crew.landed * Game.exploitationResourceModel(game.normalizeMen) * stock.extraction * avail
    val amount = Math.min(avail, theoretical.ceil.toInt)
    ExploitResult(amount = amount, r = resource)
  }

}


case class Transform(materials: Map[PrimaryResource, Int])  extends Action {

  override def computeCost(board: GameBoard, game: Game): Double = {
    val (resource, production) = produce(game)
    resource.costFunction(production, game.crew.landed)
  }

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot transform without men on land")
    materials foreach { case (res, amount) =>
      require(game.collectedResources.getOrElse(res, 0) >= amount,
              s"Cannot transform with material you do not have: [$res / $amount]")
    }
    val (k, p) = produce(game)
    TransformResult(kind = k, production = p.floor.toInt)
  }

  private def produce(game: Game): (ManufacturedResource, Double) = {
    val rawKind = Resources.manufactured.find { _.recipe.map{ _._1 } == materials.keySet }
    require(rawKind.isDefined, "Cannot transform elements according to an unknown recipe")
    val recipe = rawKind.get.recipe.toMap
    val maxProd = materials map { case (k,v) => v / recipe(k) }
    val rawProduction = maxProd.min
    val factor = 0.9 + (Random.nextDouble() / 10 * 2) // ==> factor \in [ 0.9, 1.1 ]
    (rawKind.get,rawProduction * factor)
  }

}

/**
 * Keywords to be used in JSON actions
 */
object Actions {
  final val LAND      = "land"
  final val EXPLORE   = "explore"
  final val MOVE_TO   = "move_to"
  final val SCOUT     = "scout"
  final val EXPLOIT   = "exploit"
  final val STOP      =  "stop"
  final val GLIMPSE   = "glimpse"
  final val TRANSFORM = "transform"
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