package eu.ace_design.island.map

import eu.ace_design.island.geom._
import eu.ace_design.island.map.processes.Process

/**
 * An IslandBuilder is a sequence of Process used to build an Island map
 */
trait IslandBuilder {

  // The size of the map
  def size: Int

  /**
   * Steps is the sequence (order matters) of processes used to create the map
   */
  protected val steps: Seq[Process]

  /**
   * Exploit a given mesh to build a map on top of it
   * @param m the mesh to be used as entry point
   * @return an island map, based on the sequential execution of the different steps of this builder
   */
  def apply(m: Mesh): IslandMap = {
    require(m.size.isDefined, "Cannot build an island without knowing its size")
    require(m.size.get == size, "Size of mesh and map must be consistent")
    (IslandMap(m) /: steps) { (map, p) => p(map) }
  }

}



