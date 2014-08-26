package eu.ace_design.island.map.processes

import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.util.{LogSilos, Logger}
import scala.util.Random

/**
 * A process works on a map to produce an updated map, based on its semantics. To be used as a Step in a builder.
 *
 * Remark: This trait is sealed, and cannot be implemented outside of this file.
 */
trait Process extends Logger {

  override final val silo = LogSilos.MAP_GEN

  /**
   * This function literally apply this steps on a given map, producing a new one that contains the properties
   * introduced by this process
   * @param m the input map
   * @return the enhanced map
   */
  def apply(m: IslandMap): IslandMap

}

/**
 * A process can rely on random numbers. By explicitly mixing this trait, the execution runtime ensure that the seed
 * used to build the map (if given) is propagated to the process. It then supports the reproducibility of map
 * building process, by ensuring the sequence of number generated (assuming there is no parallelism in the process)
 */
trait RandomizedProcess extends Process {

  /**
   * By default, calling a randomized process without specifying a random generator means to instantiate a new one
   * @param m the input map
   * @return the enhanced map
   */
  final def apply(m: IslandMap): IslandMap = this(new Random())(m)

  /**
   * Apply the process considering a given random number generator.
   * @param rand
   * @param m the input map
   * @return the enhanced map, using rand for random number generation
   */
  def apply(rand: Random)(m: IslandMap): IslandMap

}
