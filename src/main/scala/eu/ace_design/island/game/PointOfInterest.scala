package eu.ace_design.island.game

import scala.util.Random
import eu.ace_design.island.geom.Point

/**
 * A Point of Interest (POI) is an element available on the map
 */
trait PointOfInterest extends eu.ace_design.island.util.NameAsClassName {

  val identifier: String

  val location: Option[Point] = None

}

/**
 * A POIGenerator is used to add POIs to a given board (and can be sequenced)
 */
trait POIGenerator {
  def apply(rand: Random = new Random(), loc: TileLocator)(board: GameBoard): GameBoard
}

/**
 * this object is used to generate unique identifiers for POIs
 */
object UUIDGenerator {
  import java.util.UUID
  def apply(): String = UUID.randomUUID().toString
}

