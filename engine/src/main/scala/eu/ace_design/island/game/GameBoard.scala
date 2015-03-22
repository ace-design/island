package eu.ace_design.island.game

import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.map.resources.{Conditions, Biome, PrimaryResource, Resource}
import org.json.JSONObject


/**
 * The gameBoard is composed by a squared grid of Tiles (containing resources)
 * @param size the size of the grid (i.e., max for coordinates)
 * @param m the IslandMap used to build the board
 * @param tiles the tiles composing the grid
 * @param pois the different points of interests available on the game board
 */
case class GameBoard(size: Int, m: IslandMap,
                     tiles: Map[(Int,Int), Tile] = Map(),
                     pois:  Map[(Int,Int), Set[PointOfInterest]] = Map(),
                     tileUnit: Int = DEFAULT_TILE_UNIT) {

  /**
   * Add a location/tile couple to the current GameBoard (update if already existing)
   * @param data the couple to add
   * @return a new game board, in a functional way
   */
  def +(data: ((Int, Int), Tile)): GameBoard = this.copy(tiles = tiles + data)

  /**
   * Add a location/POI couple to the current game board
   * @param data the couple to add (meaning: POI p is located in the (x,y) tile)
   * @return a new game board, in a functional way
   */
  def addPOI(data: ((Int, Int), PointOfInterest)): GameBoard = {
    require(tiles.get(data._1._1, data._1._2).nonEmpty, s"Cannot add a POI to an non-existing location: ${data._1}")
    pois.get(data._1) match {
      case None      => this.copy(pois = pois + (data._1 -> Set(data._2)))
      case Some(set) => this.copy(pois = pois + (data._1 -> (set + data._2)))
    }
  }

  /**
   * Return the contents of the game board at a given location
   * @param x the x coordinate
   * @param y the x coordinate
   * @return the tile located at (x,y), an IllegalArgumentException elsewhere.
   */
  def at(x: Int, y: Int): Tile = {
    require(tiles.keySet.contains((x,y)), s"No tile located at ($x,$y)")
    tiles((x,y))
  }

  /**
   * Return the point of interests for a given tile
   * @param x the x coordinate
   * @param y the x coordinate
   * @return the set of POIs located here (empty set if none)
   */
  def getPOIs(x: Int, y: Int): Set[PointOfInterest] = this.pois.getOrElse((x,y), Set())

  def findPOIsByType(prototype: PointOfInterest): Set[((Int, Int), PointOfInterest)] = {
    val all = this.pois map { case (k,v) => v map { e => k -> e } }
    (all.flatten.filter { case (k,v) => v.getClass == prototype.getClass }).toSet
  }

  /**
   * Identify which resources are produced by a given tile on the board
   */
  def produces(x: Int, y: Int): Set[Resource] = {
    require(tiles.keys.toSet.contains((x,y)), "The (x,y) location must exist")
    tiles((x,y)).resources
  }

  /**
   * Return the contents (available resources and quantities) stored in this board
   * @return
   */
  def contents: Map[PrimaryResource, Int] = {
    val all = tiles.values flatMap { _.stock }
    val grouped = all groupBy { _.resource } map { case (k,v) => k -> (v map { _.amount}).sum }
    grouped
  }

  /**
   * Compute the pitch factor between two locations. if the 2 locs are at the very same altitude, the pitch factor is
   * equal to 1.
   * @param loc1
   * @param loc2
   * @return
   */
  def pitchFactor(loc1: (Int, Int), loc2: (Int, Int)): Double = {
    import eu.ace_design.island.map.resources.PIXEL_FACTOR
    val t1 = at(loc1._1, loc1._2); val t2 = at(loc2._1, loc2._2)
    val rise = Math.abs(t1.altitude - t2.altitude) // altitude already stored as meters
    val run = tileUnit.toDouble * PIXEL_FACTOR // run to be transformed from pixel to meters
    1 + ((rise/run) / 10)
  }

  def biomeFactor(loc:(Int, Int)): Double = {
    val biomes = at(loc._1, loc._2).biomes
    (biomes map { d => d._1.crossFactor * d._2}).sum
  }
}

object Directions extends Enumeration {
  type Direction = Value
  val NORTH, EAST, WEST, SOUTH = Value

  def move(x: Int, y: Int, d: Direction): (Int,Int) = d match {
    case NORTH => (x, y-1)
    case SOUTH => (x, y+1)
    case WEST  => (x-1, y)
    case EAST  => (x+1, y)
  }
}

/**
 * A tile represent the unit for moves on the board
 */
case class Tile(stock: Set[Stock] = Set(), altitude: Double = 0.0,
                biomes: Set[(Biome, Double)]= Set(), moisture: Double = 0.0) {

  /**
   * Add a given stock to the tile. Require that no stock of the very same resource is already available
   * inside the tile (see ++ to add a set of homogeneous resources)
   * @param s the stock unit to add
   * @return the tile containing the stock unit
   */
  def +(s: Stock): Tile = {
    require(! stock.exists {_.resource == s.resource }, "Cannot add an already existing stock")
    s.amount match {
      case 0 => this
      case _ => this.copy(stock = stock + s)
    }
  }

  /**
   * Add a set of homogeneous resources to the tiles. The resources are composed according to the following semantics:
   *   - the amount of the summed stock is the sum of the contents of the set
   *   - the extraction factor is the average
   * @param s  the set of stock unit to add
   * @return the tile containing the summed stock
   */
  def ++(s: Set[Stock]): Tile = s.isEmpty match {
    case true => this
    case false => {
      require(s.forall( _.resource == s.head.resource), "Cannot add a set of heterogeneous resources")
      val amount = (s map {_.amount}).sum
      val factor = (s map {_.extraction}).sum / s.size
      this + Stock(s.head.resource, amount, factor)
    }
  }

  /**
   * Add a set of heterogeneous resources, supporting multiple instances of the same resources in ≠ stock unit.
   * @param s
   * @return
   */
  def bulkAdd(s: Set[Stock]): Tile =(this /: (s groupBy { _.resource }).values) { (acc, elem) => acc ++ elem }

  /**
   * the resources produced by this tile
   * @return
   */
  def resources: Set[Resource] = stock map { _.resource }

  def diffAltitude(that: Tile): Int = (that.altitude - this.altitude).ceil.toInt
  
}

case class Stock(resource: PrimaryResource, amount: Int, extraction: Double = 1.0) {
  require(amount >= 0, "Cannot hold negative value in a Stock")
  require(extraction > 0, "the extraction factor must be positive")

  def explore(board: GameBoard, harvested: Int): (ResourceLevels.ResourceLevel, Conditions.Condition) = {
    import eu.ace_design.island.map.resources.PIXEL_FACTOR
    val maxPerHa = resource.perHectare.toDouble
    val tileSize = board.tileUnit *  PIXEL_FACTOR * board.tileUnit * PIXEL_FACTOR
    val maxPerTile = maxPerHa * tileSize / 10000  // 1ha = 10,000 squared meters
    val level = amount - harvested match {
      case a if a <= maxPerTile / 3      => ResourceLevels.LOW
      case a if a >= 2 * maxPerTile / 3  => ResourceLevels.HIGH
      case _                             => ResourceLevels.MEDIUM
    }
    val condition = extraction match {
      case e if e <= 0.6 => Conditions.HARSH
      case e if e >= 1.3 => Conditions.EASY
      case _ => Conditions.FAIR
    }
    (level, condition)
  }

}

object ResourceLevels extends Enumeration {
  type ResourceLevel = Value ; val HIGH, MEDIUM, LOW = Value
}
