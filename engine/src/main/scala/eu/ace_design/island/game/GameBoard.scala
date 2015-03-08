package eu.ace_design.island.game

import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.map.resources.{Biome, PrimaryResource, Resource}


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
   * Compute the neighbors (existing tiles) for a given location.
   * @param x the x coordinate
   * @param y the y coordinate
   * @return the set of Direction leading to existing neighbors
   */
  def neighbors(x: Int, y: Int): Set[Directions.Direction] = {
    val candidates = Directions.values map { dir => dir -> Directions.move(x,y,dir) }
    val existing = candidates.filter { case (_, (u,v)) =>
      try { at(u,v); true } catch { case e: IllegalArgumentException => false }
    }
    existing map { _._1 }
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
}

object Directions extends Enumeration {
  type Direction = Value
  val NORTH, EAST, WEST, SOUTH = Value

  def move(x: Int, y: Int, d: Direction): (Int,Int) = d match {
    case NORTH => (x-1, y)
    case SOUTH => (x+1, y)
    case WEST  => (x,   y-1)
    case EAST  => (x,   y+1)
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
  
}

case class Stock(resource: PrimaryResource, amount: Int, extraction: Double = 1.0) {
  require(amount >= 0, "Cannot hold negative value in a Stock")
  require(extraction > 0, "the extraction factor must be positive")
}