package eu.ace_design.island.game.actions

import eu.ace_design.island.game._

case class Scan () extends Action {

  override def computeCost(board: GameBoard, game: Game): Double = 1.0

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.plane.isDefined, "Cannot scan without a plane")
    val data = game.plane.get.snapshot(board)
    ScanResult(biomes = data._1, creeks = data._2, scanned = data._3)
  }
}

