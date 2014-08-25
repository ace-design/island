package eu.ace_design.island.map.processes

import eu.ace_design.island.map._

/**
 * This process computes for each land vertex v the minimal distance to reach a coastal vertex from v. The distance is
 * computed as absolute, in the size x size space.
 *
 * This process only involves the vertices of the mesh.
 *
 * Pre-conditions:
 *   - Coast vertices are identified by IsCoast(true)
 *   - land vertices are identified by IsWater(false)
 *
 * Post-condition:
 *   - each land vertices is annotated with DistanceToCoast(d), with d in [0,size].
 *
 **/
object MinimalDistanceToCoast extends Process {

  override def apply(m: IslandMap): IslandMap = {
    info("Computing minimal distance to coast for land vertices")
    val coast = m.findVerticesWith(Set(IsCoast()))
    val land =  m.findVerticesWith(Set(!IsWater()))
    // TODO coastal vertices can be precomputed as 0.0
    // computing distances
    val distances = (m.vertexProps /: land) { (acc, point) =>
      val distance = (coast map { point --> _ }).min  // finding the minimal one
      acc + (m.vertexRef(point) -> DistanceToCoast(distance))
    }
    m.copy(vertexProps = distances)
  }

}
