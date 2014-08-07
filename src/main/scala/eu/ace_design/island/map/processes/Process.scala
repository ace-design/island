package eu.ace_design.island.map.processes

import eu.ace_design.island.map.IslandMap

/**
 * A process works on a map to produce an updated map, based on its semantics. To be used as a Step in a builder.
 *
 * Remark: This trait is sealed, and cannot be implemented outside of this file.
 */
trait Process {

  /**
   * This function literally apply this steps on a given map, producing a new one that contains the properties
   * introduced by this process
   * @param m the input map
   * @return the enhanced map
   */
  def apply(m: IslandMap): IslandMap

  // TODO introduce a mechanism to expose properties requirements (e.g., this process assume p to be present)
}
