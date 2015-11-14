package eu.ace_design.island.game

import eu.ace_design.island.map.resources.{ManufacturedResource, PrimaryResource, Resource}
import eu.ace_design.island.stdlib.Biomes
import eu.ace_design.island.util.Polynomial

/**
 * A game is used to store the information about a given game
 * @param budget the budget available to play with
 * @param crew  the crew available to explore the island
 * @param objectives the objectives given by the
 * @param visited the locations visited by the explorer
 * @param boat The location of the boat
 * @param isOK is the game ok ? Setting this to false stops the game engine
 * @param extracted the resources extracted from the map, tile per tile
 * @param consumed the amount of resource consumed to perform resource transformation
 * @param transformed the amount of resources obtained through transformation
 */
class Game private(val budget: Budget,
                   val crew: Crew,
                   val objectives: Set[(Resource, Int)],
                   val visited: Set[(Int, Int)],
                   val boat: Option[(Int, Int)],
                   val plane: Option[Plane],
                   val isOK: Boolean = true,
                   val extracted: Map[Resource,Map[(Int,Int),Int]] = Map(),
                   val consumed: Map[PrimaryResource, Int] = Map(),
                   val transformed: Map[ManufacturedResource, Int] = Map()) {

  /**
   * Update the current game based on the contents of the result of an action
   * @param res
   * @return a game
   */
  def updatedBy(res: Result): (Game, Result) = res.ok match {
    case false => (this, res)
    case true => {
      val g = res match {
        case e: EmptyResult   => this // no side effect (except on budget)
        case s: ScoutResult   => this // no side effect (except on budget)
        case e: ExploreResult => this // no side effect (except on budget)
        case g: GlimpseResult => this // no side effect (except on budget)
        case m: MovedBoatResult => {
          val updatedCrew = crew movedTo m.loc using m.men
          this.copy(crew = updatedCrew, boat = Some(m.loc), visited = visited + m.loc)
        }
        case  m: MovedCrewResult => {
          val updatedCrew = crew movedTo m.loc
          this.copy(crew = updatedCrew, visited = visited + m.loc)
        }
        case m: MovedPlaneResult => {
          val updatedPlane = plane.get.copy(position = m.planeLoc)
          this.copy(plane = Some(updatedPlane))
        }
        case e: ExploitResult => harvest(e.r, crew.location.get, e.amount)
        case t: TransformResult => {
          val primaryRemoved = (this /: t.consumed) { (g, cons) => g.consumeResource(cons._1, cons._2) }
          primaryRemoved.storeTransformedResources(t.kind, t.production)
        }
        case _ => throw new UnsupportedOperationException("Game cannot handle update with " + res)
      }
      val remaining = budget - res.cost
      (g.copy(budget = remaining), res)
    }
  }

  /**
   * Identify the amount of resources harvested on a given tile
   * @param r the resource to work with
   * @param loc the location of the tile
   * @return an integer value stating how many units of this resource was extracted
   */
  def harvested(r: Resource, loc: (Int, Int)): Int = {
    val forResource = extracted.getOrElse(r,Map())
    forResource.getOrElse(loc,0)
  }

  /**
   * Harvest a given quantity of resource on the game
   * @param r the resource to harvest
   * @param loc tile location
   * @param amount amount of resource to harvest
   * @return
   */
  def harvest(r: Resource, loc: (Int, Int), amount: Int): Game = {
    val updated = extracted.get(r) match {
      case None => extracted + (r -> Map(loc -> amount))
      case Some(data) => data.get(loc) match {
        case None => extracted + (r -> (data + (loc -> amount)))
        case Some(old) => extracted + (r -> (data + (loc -> (old + amount))))
      }
    }
    this.copy(extracted = updated)
  }

  /**
   * Compute the collected resources for a player. This is built as the following :
   *   collected = resources harvested on the map - resources consumed by transformation + manufactured resources
   * @return
   */
  def collectedResources: Map[Resource, Int] = {
    val harvested = this.extracted map { case (resource, data) => resource -> data.values.sum }
    val withConsumed =
      harvested map { case (res, collected) => res -> (collected - consumed.getOrElse(res.asInstanceOf[PrimaryResource], 0)) }
    withConsumed ++ transformed
  }

  /**
   * Compute the distance from the boat location and the port of origin
   * @return Some(x) where x is the distance, and None if the boat did not move before the beginning of the game
   */
  def distanceToPort: Option[Double] = this.boat match {
    case None => None
    case Some(boatLoc) => Some(distance((0,0), boatLoc))
  }

  /**
   * Compute the distance between the team and the boat
   * //TODO: explore the visited tiles to find the shortest path
   * @return
   */
  def distanceToBoat: Double = this.crew.location match {
    case None => 0.0
    case Some(teamLoc) => distance(boat.get,teamLoc) // Assumption: crew.location ≠ None => boat location ≠ None
  }

  /**
   * returns the men ratio used in the cost model
   * @return
   */
  def menRatio: Double = this.crew.landed * Game.MEN_RATIO

  /**
   * Normalize the number of mens on the island (project it into [0,1])
   * @return
   */
  def normalizeMen: Double = Math.min(1.0, crew.landed / Game.MEN_NORMALIZE_THRESHOLD)

  /**
   * compute the distance between a destination and the current location of the boat
   * @param destination
   * @return
   */
  def distanceByBoat(destination: (Int, Int)): Double = distance(boat.getOrElse((0,0)), destination)

  // compute the distance between two points
  private def distance(a: (Int, Int), b: (Int, Int)): Double =
    Math.sqrt(Math.pow(a._1 - b._1,2) + Math.pow(a._2 - b._2,2))

  /**
   * Quickly tag a game as KO by changing its status
   * @return
   */
  def flaggedAsKO: Game = this.copy(isOK = false)

  /**
   * Consumes resources on the board
   */
  def consumeResource(res: PrimaryResource, amount: Int): Game = {
    require(collectedResources.getOrElse(res,0) >= amount, "Cannot consume resource you do not have")
    copy(consumed = consumed + (res -> (consumed.getOrElse(res,0) + amount)))
  }

  // Store transformed resources in the ship hold
  def storeTransformedResources(res: ManufacturedResource, amount: Int): Game = {
    val legacy = transformed.getOrElse(res,0)
    copy(transformed = transformed + (res -> (legacy + amount)))
  }

  // copy a game into another one (simulating case class behavior)
  def copy(budget: Budget = this.budget, crew: Crew = this.crew,
                   objectives: Set[(Resource, Int)] = this.objectives,
                   visited: Set[(Int, Int)] = this.visited, boat: Option[(Int, Int)] = this.boat,
                   plane: Option[Plane] = this.plane, isOK: Boolean = this.isOK,
                   extracted: Map[Resource,Map[(Int,Int),Int]] = this.extracted,
                   consumed: Map[PrimaryResource, Int] = this.consumed,
                   transformed: Map[ManufacturedResource, Int] = this.transformed) =
    new Game(budget, crew, objectives, visited, boat, plane, isOK, extracted, consumed, transformed)

}

/**
 * Global information about Games
 */
object Game {

  // ratio used to work on the number of mens available on the island
  final val MEN_RATIO: Double = 1.0 / 5.0

  final val MEN_NORMALIZE_THRESHOLD = 50.0

  def apply(budget: Budget, crew: Crew, objectives: Set[(Resource, Int)]) =
    new Game(budget,crew, objectives, visited = Set(), boat = None, plane = None, true)

  // Models (polynomials) used for resource exploitation and cost definition

  val exploitationCostModel     = Polynomial(Seq(1, -1.0857, 0,6857))
  val exploitationResourceModel = Polynomial(Seq(0, 2,8571, -2.0751))
  val movingCostModel           = Polynomial(Seq(0, 3.2476, -6.5143, 4.2667))

}

/**
 * Represent the
 * @param initial
 * @param position
 * @param heading
 */
class Plane private(val initial: (Int, Int), val position: (Int, Int), val heading: Directions.Direction) {

  /**
    * Computes the ounding box of the plane, that is, the tiles available under the plane
    * @return
    */
  def boundingBox: Set[(Int,Int)] =
    (for(x <- position._1 - 1 to position._1 + 1; y <- position._2 - 1 to position._2 + 1 ) yield (x,y)).toSet

  /**
    * Make the plane fly forward
    * @return a plane with updated location
    */
  def forward: Plane = {
     this.copy(position = Directions.move(position._1, position._2, heading, Plane.MOVE))
  }


  /**
    * Make the plane change heading while flying forward
    * @param d
    * @return
    */
  def turn(d: Directions.Direction): Plane = {
    require(Directions.orthogonal(this.heading).contains(d), s"Cannot turn [${d}] when heading [${heading}]")
    forward.copy(heading = d).forward    // go straight, turn, go straight again
  }

  /**
    * Send a radar signal to a given direction, identifying ground tiles or out of range limit.
    * /!\ Warning: No radar at the rear of the plane
    * @param d the direction of the radar
    * @param board the underlying board
    * @return a couple (d,K) where r is the distance (number of tiles / 3) and K the encountered kind of element
    */
  def radar(d: Directions.Direction, board: GameBoard): (Int, RadarValue.Value) = {

    // return the number of tiles (a range) between x,y and the end of the map or something which is not the Ocean
     def signal(x: Int, y: Int, range: Int = 0): (Int, RadarValue.Value) = {
      if (! board.tiles.keySet.contains((x,y))) {
        (range, RadarValue.OUT_OF_RANGE)
      } else {
        val biomes = board.tiles.get((x,y)).get.biomes map { _._1 }
        if (! biomes.contains(Biomes.OCEAN)) {
          (range, RadarValue.GROUND)
        } else {
          val (nX, nY) = Directions.move(x,y,d)
          signal(nX,nY,range + 1)
        }
      }
    }

    require(d != Directions.opposite(this.heading), s"No radar for [${d}] when heading [${heading}]")
    //moving the pointer out of the current bounding box of the plane
    val (tX,tY) = Directions.move(position._1, position._2,d)
    val (sX,sY) = Directions.move(tX, tY,d)
    // Send up to 3 signals if possible
    val others = Directions.orthogonal(d) map { Directions.move(sX,sY,_) } filter {
      case (x,y) => board.tiles.keySet.contains((x,y))
    }
    // Sending the signals
    val raw = (others + ((sX,sY))) map { case (x,y) => signal(x,y) }
    // finding the minimal return result
    val minimal = raw minBy { case (i,_) => i }
    (minimal._1 / 3, minimal._2)
  }

  def copy(initial: (Int, Int) = initial, position: (Int, Int) = position, heading: Directions.Direction = heading) = {
    new Plane(initial, position, heading)
  }

}

object RadarValue extends Enumeration {
  val GROUND, OUT_OF_RANGE = Value
}

object Plane {

  private val MOVE = 3

  def apply(x: Int, y: Int, h: Directions.Direction) = new Plane((x,y), (x,y), h)
}

/**
 * A budget represent the complete amount of action points available, and the remaining ones.
 */
class Budget private(val initial: Int, val remaining: Int) {
  require(initial > 0, "Initial budget cannot be negative")

  /**
   * Spend action points in the budget. Throw a NotEnoughBudgetException if necessary
   * @param cost the number of action points to be removed
   * @return
   */
  def -(cost: Int): Budget = {
    val r = this.remaining - cost
    if (r < 0)
      NotEnoughBudgetException(remaining, cost) // error case
    new Budget(initial, r)
  }
}

/**
 * Object use to mimic the instantiation of a case class for Budget
 */
object Budget {
  def apply(value: Int) = new Budget(value, value)
}

class NotEnoughBudgetException(message: String) extends Exception(message)
object NotEnoughBudgetException {
  /**
   * Syntactic sugar to throw an exception
   * @param avail  remaining budget
   * @param needed needed action points
   * @return nothing (throws an exception)
   */
  def apply(avail: Int, needed: Int) = {
    val m = s"Not enough budget to do that! Needed $needed, Available $avail"
    throw new NotEnoughBudgetException(m)
  }
}

/**
 * A crew represents the number of men available on the boat, the number of men used for the exploration and the
 * number of men currently on the island
 */
class Crew private(val complete: Int, val used: Int, val landed: Int, val location: Option[(Int, Int)]) {
  require(complete > 1, "Not enough men in the crew")

  /**
   * Update a crew by landing a given amount of people, counting how many folks where used.
   * @param m number of men to use
   * @return a copy of this, with updated values
   */
  def using(m: Int) = this.copy(used = used + m, landed = m)

  /**
   * move the crew on the island
   * @param loc
   * @return
   */
  def movedTo(loc: (Int, Int)) = this.copy(location = Some(loc))

  /**
   * private copy constructor
   * @return
   */
  private def copy(complete: Int = this.complete,  used: Int = this.used,  landed: Int = this.landed,
                   location: Option[(Int, Int)] = this.location) = new Crew(complete, used, landed, location)

}
object Crew { def apply(men: Int) = new Crew(men, 0, 0, None) }