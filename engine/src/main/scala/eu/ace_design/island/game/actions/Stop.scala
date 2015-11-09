package eu.ace_design.island.game.actions

import eu.ace_design.island.game._

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
