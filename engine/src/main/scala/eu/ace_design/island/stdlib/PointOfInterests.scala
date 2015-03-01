package eu.ace_design.island.stdlib

import eu.ace_design.island.game._
import eu.ace_design.island.geom.Point
import eu.ace_design.island.map.{IsCoast, IslandMap}
import eu.ace_design.island.stdlib.PointOfInterests.Creek

import scala.util.Random

/**
 * Standard element to be used as Point of interests
 **/
object PointOfInterests {

  /**
   * A port is used to support the landing operation
   * @param identifier the identifier of this port
   */
  case class Creek(override val identifier: String,
                   override val location: Option[Point]) extends PointOfInterest {}

  case class Hideout(override val identifier: String,
                     override val location: Option[Point]) extends PointOfInterest {}

}


/**
 * POIGenerators are function used in the GameBoardBuilder to introduce Point of Interest in a board
 */
object POIGenerators {

  /**
   * This class introduce ports in the board
   * @param howMany number of ports to add
   */
  class WithCreeks(howMany: Int) extends POIGenerator {

    override def apply(rand: Random = new Random(), loc: TileLocator)(board: GameBoard): GameBoard = {
      // find locations:
      val coasts = board.m.findVerticesWith(Set(IsCoast())).toSeq
      // instantiate ports
      val ports = (0 until howMany) map { i =>
        val idx = rand.nextInt(coasts.size-1)
        loc(coasts(idx)) -> Creek(UUIDGenerator(), Some(coasts(idx)))
      }
      // enrich the board
      (board /: ports) { (acc, poi) => acc addPOI poi }
    }
  }
}