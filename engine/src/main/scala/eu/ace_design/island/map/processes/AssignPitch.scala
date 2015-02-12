package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.Face
import eu.ace_design.island.map.{HasForPitch, HasForHeight, IslandMap}

/**
 * This process assign to each face the associated pitch (declivity).
 *
 * Pre-conditions:
 *   - HasForHeight defined over vertices
 *
 * Post-conditions:
 *   - HasForPitch defined for faces
 *
 * ref: http://en.wikipedia.org/wiki/Grade_(slope)#mediaviewer/File:Grades_degrees.svg
 *
 */
object AssignPitch extends Process {

  override def apply(m: IslandMap): IslandMap = {
    info("Computing faces pitches")
    val elevations = m.vertexProps.restrictedTo(HasForHeight())
    val pitches = (m.faces map { f => m.faceRef(f) -> compute(f,m,elevations) }).toMap
    val props = (m.faceProps /: pitches) { (acc, pair) => acc + (pair._1 -> HasForPitch(pair._2)) }
    m.copy(faceProps = props)
  }

  /**
   * Compute the pitch (in percentage) for a given face.
   * Illustration: http://en.wikipedia.org/wiki/Grade_(slope)
   *
   * @param face the face to handle
   * @param m the map to compute the run (distance between highest and lowest corners with z = 0)
   * @param elevations a map of elevation (to compute the rise, z difference between highest and lowest corners)
   * @return the pitch as a percentage. a pitch of 100% is equivalent to an angle of 45 degrees.
   */
  private def compute(face: Face, m: IslandMap, elevations: Map[Int, Double]): Double = {
    val corners = m.cornerRefs(face) map { c => c -> elevations.getOrElse(c,0.0) }
    val lowest  = corners minBy  { _._2 }
    val highest = corners maxBy  { _._2 }
    val run     = m.vertex(lowest._1) --> m.vertex(highest._1)
    val rise    = highest._2 - lowest._2
    if (rise == 0) 0.0 else 100.0 * rise / run
  }

}
