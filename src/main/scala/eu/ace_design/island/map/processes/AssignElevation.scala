package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.EdgeRegistry
import eu.ace_design.island.map.{PropertySet, HasForHeight, IsBorder, IslandMap}


/**
 * This process assigns an elevation (z coordinate) to the vertices stored in the map, according to a given function.
 * The function used as parameter is applied to the distance to the coast of each corner. Contrarily to the others,
 * vertices used as face' center as for elevation the average of the elevation of each vertex used in the border of
 * the face.
 *
 * The average elevation of a face is then assimilated to the elevation of its center.
 *
 * Pre-conditions:
 *   - The map contains "DistanceToCoast(d)" annotations for each land (i.e., !ocean) vertices.
 *
 * Post-conditions:
 *   - All vertices are  tagged with "HasForHeight(h)", where h is the elevation.
 *   - All faces are tagged with "HasForHeight(h)", where h is the height of its center face (shortcut)
 */
case class AssignElevation(phi: ElevationFunctions.ElevationFunction) extends Process {

  override def apply(m: IslandMap): IslandMap = {
    info("Assigning initial elevation")


    m

  }
}


object ElevationFunctions {

  type ElevationFunction =  Map[Int,Double] => Map[Int,Double]

  def identity: ElevationFunction = m => m

  def peak(summit: Int): ElevationFunction = distances => {
    val max = distances.values.max
    distances map { case (key, distance) => key -> distance / max * summit }
  }

}
