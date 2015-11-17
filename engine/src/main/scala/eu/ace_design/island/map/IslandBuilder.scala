package eu.ace_design.island.map

import eu.ace_design.island.geom._
import eu.ace_design.island.map.processes.{RandomizedProcess, Process}
import scala.util.Random

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
  def apply(m: Mesh, random: Random = new Random()): IslandMap = {
    require(m.size.isDefined, "Cannot build an island without knowing its size")
    require(m.size.get == size, "Size of mesh and map must be consistent")
    (IslandMap(m) /: preProcess(random, steps)) { (map, p) => p(map) }
  }

  /**
   * Propagate a shared random generator to each process when needed (i.e., a process p is a Randomized one)
   * @param random the random generator to be used
   * @param steps the processes to pre-process.
   * @return a sequence of function to be used to transform a given map into an updated one.
   */
  def preProcess(random: Random, steps: Seq[Process]): Seq[IslandMap => IslandMap] = {
    steps map { step => step  match {
      case randomized: RandomizedProcess => randomized(random) _
      case deterministic: Process        => deterministic.apply _
    } }
  }

}



