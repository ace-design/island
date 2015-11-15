package eu.ace_design.island.game.actions

import eu.ace_design.island.game.Directions.Direction
import eu.ace_design.island.game._

case class Heading(override val direction: Direction) extends ActionWithDirection {

  override def computeCost(board: GameBoard, game: Game): Double = 1.0

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.plane.isDefined, "Cannot fly without a plane")
    val next = game.plane.get.turn(direction).position
    require(board.tiles.keySet.contains(next), "Congrats, the plane is out of radio range...")
    MovedPlaneResult(planeLoc = next)
  }

}
