package eu.ace_design.island.game.actions

import eu.ace_design.island.game._

import scala.util.Random

/**
 * This file is part of the Default (Template) Project project
 * @author mosser (26/10/2015, 17:36)
 **/
// { "action": "explore" }
case class Explore() extends Action {

  override def computeCost(board: GameBoard, game: Game): Double = {
    val biomeFactor = board.biomeFactor(game.crew.location.get)
    val factor = (biomeFactor + Game.movingCostModel(game.normalizeMen)) / 2.0
    (1 + Random.nextInt(5)) * factor
  }

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot explore without having landed before")
    val current = game.crew.location.get  // cannot be None as game.boat is not empty
    val tile = board.at(current._1, current._2)
    val pois = board.pois.getOrElse((current._1, current._2), Seq()).toSet
    val resources = tile.stock map { stock =>
      val (lvl, cond) = stock.explore(board, game.harvested(stock.resource, game.crew.location.get))
      ResourceExploration(stock.resource, lvl, cond)
    }
    ExploreResult(resources = resources, pois = pois)
  }
}
