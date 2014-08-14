package eu.ace_design.island.map.processes

import eu.ace_design.island.map._

/**
 * This process computes for each land vertex v the minimal distance to reach a coastal vertex from v. The distance is
 * computed as relative (in [0,1]) between the minimal distance (coast vertex) and the maximal one (far far away vertex)
 *
 * This process only involves the vertices of the mesh.
 *
 * Pre-conditions:
 *   - Coast vertices are identified by IsCoast(true)
 *   - land vertices are identified by IsWater(false)
 *
 * Post-condition:
 *   - each land vertices is annotated with DistanceToCoast(d), with d in [0,1].
 *
 **/
object MinimalDistanceToCoast extends Process {

  override def apply(m: IslandMap): IslandMap = {
    val props =  m.vertexProps.project(m.mesh.vertices) _
    val coast = props(Set(IsCoast()))
    val land =  props(Set(!IsWater()))
    // TODO coastal vertices can be precomputed as 0.0
    // computing distances
    val distances = (Map[Int,Double]() /: land) { (acc, point) =>
      val distance = (coast map { point --> _ }).min  // finding the minimal one
      val ref = m.mesh.vertices(point).get
      acc + (ref -> distance)
    }
    // normalizing distances
    val max = distances.values.max
    val withDistance = (m.vertexProps /: distances) { (acc, pair) =>
      acc + (pair._1 -> DistanceToCoast(pair._2 / max) )
    }
    m.copy(vertexProps = withDistance)
  }

}
