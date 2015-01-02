package eu.ace_design.island.game

import eu.ace_design.island.map.resources.ExistingResources.Resource

/**
 * The gameBoard is composed by a squared grid of Tiles (containing resources)
 * @param size the size of the grid (i.e., max for coordinates)
 * @param tiles the tiles composing the grid
 */
case class GameBoard(size: Int, tiles: Map[(Int,Int), Tile] = Map()) {

  /**
   * Add a location/tile couple to the current GameBoard (update if already existing)
   * @param data the couple to add
   * @return a new game board, in a functional way
   */
  def +(data: ((Int, Int), Tile)): GameBoard = {
    val updated = tiles.get(data._1) match {
      case None    => tiles + data
      case Some(_) => tiles - data._1 + data
    }
    this.copy(tiles = updated)
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
    tiles((x,y)).stock map { _.resource }
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
case class Tile(stock: Set[Stock] = Set()) {

  def +(s: Stock): Tile = {
    require(! stock.exists {_.resource == s.resource }, "Cannot add an already existing stock")
    this.copy(stock = stock + s)
  }

  def ++(s: Set[Stock]): Tile = {
    this.copy(stock = stock ++ s)
  }

}

case class Stock(resource: Resource, amount: Int, extraction: Double = 1.0) {
  require(amount >= 0, "Cannot hold negative value in a Stock")
  require(extraction > 0, "the extraction factor must be positive")
}