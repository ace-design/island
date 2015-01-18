package eu.ace_design.island.stdlib

import eu.ace_design.island.game.{TileLocator, POIDispatcher, PointOfInterest}
import eu.ace_design.island.map.{IsCoast, IslandMap}

import scala.util.Random

/**
 * Standard element to be used as Point of interests
 **/
object PointOfInterests {

  /**
   * A port is used to support the landing operation
   * @param identifier the identifier of this port
   */
  case class Port(override val identifier: String) extends PointOfInterest {}

}


object Dispatchers {

  object COAST_LINE extends POIDispatcher {

    override def findPosition(loc: TileLocator, rand: Random = new Random())(m: IslandMap, howMany: Int = 1):
        Set[(Int, Int)] = {
      val coasts = m.findVerticesWith(Set(IsCoast())).toSeq
      val vertices = rand.shuffle(coasts).slice(0,howMany).toSet
      vertices map { loc(_) }
    }

  }

}
