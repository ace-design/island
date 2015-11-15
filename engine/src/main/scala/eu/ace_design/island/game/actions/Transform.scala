package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import eu.ace_design.island.map.resources.{ManufacturedResource, PrimaryResource}
import eu.ace_design.island.stdlib.Resources

import scala.util.Random

/**
 * This file is part of the Default (Template) Project project
 * @author mosser (26/10/2015, 17:36)
 **/
case class Transform(materials: Map[PrimaryResource, Int])  extends Action {

  override def computeCost(board: GameBoard, game: Game): Double = {
    val (resource, production) = produce(game)
    resource.costFunction(production, game.crew.landed)
  }

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot transform without men on land")
    materials foreach { case (res, amount) =>
      require(game.collectedResources.getOrElse(res, 0) >= amount,
              s"Cannot transform with material you do not have: [$res / $amount]")
    }
    val (k, p) = produce(game)
    TransformResult(kind = k, production = p.floor.toInt, consumed = materials)
  }

  private def produce(game: Game): (ManufacturedResource, Double) = {
    val rawKind = Resources.manufactured.find { _.recipe.map{ _._1 } == materials.keySet }
    require(rawKind.isDefined, "Cannot transform elements according to an unknown recipe")
    val recipe = rawKind.get.recipe.toMap
    val maxProd = materials map { case (k,v) => v / recipe(k) }
    val rawProduction = maxProd.min
    val factor = 0.9 + (Random.nextDouble() / 10 * 2) // ==> factor \in [ 0.9, 1.1 ]
    (rawKind.get,rawProduction * factor)
  }

}
