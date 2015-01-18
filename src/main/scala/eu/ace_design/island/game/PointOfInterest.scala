package eu.ace_design.island.game

import eu.ace_design.island.map.IslandMap

import scala.util.Random


/**
 * A Point of Interest (POI) is an element available on the map
 */
trait PointOfInterest extends eu.ace_design.island.util.NameAsClassName {

  val identifier: String

}

/**
 * A POIDispatcher is used to find the position of a
 */
trait POIDispatcher {
  def findPosition(loc: TileLocator, rand: Random = new Random())(m: IslandMap, howMany: Int = 1): Set[(Int, Int)]
}



/**
 * this object is used to generate unique identifiers for POIs
 */
object UUIDGenerator {
  import java.util.UUID
  def apply(): String = UUID.randomUUID().toString
}

