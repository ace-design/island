package eu.ace_design.island.game.actions

import eu.ace_design.island.game._

case class Fly() extends Action {

  /**
    * Cost of the flying action
    * @param board the game board
    * @param game the game used to store the contents of the exploration
    * @return an integer value
    */
  override def computeCost(board: GameBoard, game: Game): Double = 0

  /**
    * This method computes the result of the execution for this action
    * @param board the game board
    * @param game the game used to store the contents of the exploration
    * @return an dedicated result for this specific action
    */
  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.plane.isDefined, "Cannot fly without a plane")
    val next = game.plane.get.forward.position
    require(board.tiles.keySet.contains(next), "Congrats, the plane is out of radio range...")
    MovedPlaneResult(planeLoc = next)
  }

}
