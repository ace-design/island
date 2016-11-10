package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import eu.ace_design.island.stdlib.PointOfInterests.Creek

case class Scan() extends Action {

  override def computeCost(board: GameBoard, game: Game): Double = 1.0

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.plane.isDefined, "Cannot scan without a plane")
    val data = game.plane.get.snapshot(board)
    val pois = data._2 partition { p => p.isInstanceOf[Creek] }
    ScanResult(biomes = data._1, creeks = pois._1, sites = pois._2, scanned = data._3)
  }
}

