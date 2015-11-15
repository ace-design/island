package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import eu.ace_design.island.stdlib.PointOfInterests.Creek

/**
 * The Land action
 * @param creek
 * @param people
 */
case class Land(creek: String, people: Int) extends Action          {
  require(people > 0, "Cannot land less than one man")
  override def computeCost(board: GameBoard, game: Game): Double = {
    val creekLocation = board.findPOIsByType(Creek(null, null)).find { case (loc,c) => c.identifier == creek }.get._1
    // TODO update
    val comingBack = game.menRatio * Game.movingCostModel(game.normalizeMen) * game.distanceToBoat
    val landing = people / Game.MEN_RATIO  * Game.movingCostModel(game.normalizeMen)
    val moving = Math.sqrt(game.distanceByBoat(creekLocation))
    moving + comingBack + landing
  }


  override def buildResult(board: GameBoard, game: Game): Result = {
    require(people < game.crew.complete, "At least one men must stay on board")
    val creekData = board.findPOIsByType(Creek(null, null)) find { case (loc,c) => c.identifier == creek }
    require(creekData.isDefined, s"Unknown creek identifier [$creek]")
    val location = creekData.get._1
    MovedBoatResult(loc = location, men = people)
  }

}
