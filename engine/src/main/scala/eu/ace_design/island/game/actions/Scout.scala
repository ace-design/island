package eu.ace_design.island.game.actions

import eu.ace_design.island.game._

import scala.util.Random

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
