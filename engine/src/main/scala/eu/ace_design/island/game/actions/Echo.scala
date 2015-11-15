package eu.ace_design.island.game.actions

import eu.ace_design.island.game._

class Echo(override val direction: Directions.Direction) extends ActionWithDirection {

  override def computeCost(board: GameBoard, game: Game): Double = 1.0

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.plane.isDefined, "Cannot echo without a plane")
    val data = game.plane.get.radar(direction, board)
    EchoResult(range = data._1, found = data._2)
  }
}
