package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{HasForArea, IslandMap}

/**
 * The ComputeArea Process compute the area of each face based on the polygon present in the mesh.
 */
object ComputeArea extends Process {

  override def apply(m: IslandMap): IslandMap = {

    val props = (m.faceProps /: m.faceRefs) { (acc, ref) => acc + (ref -> HasForArea(process(ref))) }
    m

  }


  private def process(ref: Int): Double = 0.0

}
