package eu.ace_design.island.game.actions

import eu.ace_design.island.game._

/**
 * The move to action make the crew move to another tile on the map
 * @param direction
 */
case class MoveTo(override val direction: Directions.Direction) extends ActionWithDirection {

  override def computeCost(board: GameBoard, game: Game): Double = {
    val movingRawFactor = 1 + Game.movingCostModel(game.normalizeMen) // [0,1] => [0,1]
    val current = game.crew.location.get; val next = nextLoc(game)
    val pitchFactor = board.pitchFactor(current, next)
    val biomeFactor = (board.biomeFactor(current) + board.biomeFactor(next)) / 2
    val factor = (2*movingRawFactor + pitchFactor + 2*biomeFactor) / 5
    val result = game.menRatio * factor
    result
  }

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot move without having landed before")
    val next = nextLoc(game)
    require(board.tiles.keySet.contains(next), "Congrats, you just fall out of the world limit")
    MovedCrewResult(loc = next)
  }
}
