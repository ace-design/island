package eu.ace_design.island.stdlib

import eu.ace_design.island.game._
import eu.ace_design.island.geom.Point
import eu.ace_design.island.map._
import eu.ace_design.island.stdlib.PointOfInterests.{Creek, EmergencySite}

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

  case class EmergencySite(override val identifier: String,
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
      val ports: IndexedSeq[((Int, Int), Creek)] = (0 until howMany) map { i =>
        val idx = rand.nextInt(coasts.size-1)
        loc(coasts(idx)) -> Creek(UUIDGenerator(), Some(coasts(idx)))
      }
      // enrich the board
      (board /: ports) { (acc, poi) => acc addPOI poi }
    }
  }

  object WithEmergencySite extends POIGenerator {

    // We generate one emergency site near the coast
    override def apply(rand: Random = new Random(), loc: TileLocator)(board: GameBoard): GameBoard = {
      val faceRef = emergencyFaceAsBeach(board, rand) match {
        case None => emergencyFaceAsHighFace(board, rand) match {
          case None => throw new POIException("Unable to find a face for the emergency rescue site")
          case Some(ref) => ref
        }
        case Some(ref) => ref
      }
      val emergencyPoint = board.m.vertex(board.m.face(faceRef).center)

      board addPOI (loc(emergencyPoint) -> EmergencySite(UUIDGenerator(),Some(emergencyPoint)))
    }

    private def emergencyFaceAsBeach(board: GameBoard, rand: Random): Option[Int] = {
      (board.m.faceProps.restrictedTo(HasForBiome()) filter { case (_, b) =>  b == Biomes.BEACH }).toList match {
        case Nil => None
        case list => Some(list(rand.nextInt(list.size))._1)
      }
    }

    private def emergencyFaceAsHighFace(board: GameBoard, rand: Random): Option[Int] = {
      val dataset = board.m.faceProps.restrictedTo(HasForHeight())
      val max = dataset.values.max
      (dataset filter { case (_, h) =>  h <= max * 0.4 }).toList match {
        case Nil => None
        case list => Some(list(rand.nextInt(list.size))._1)
      }
    }

  }
}

class POIException(message: String) extends Exception(message)
