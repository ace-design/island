package eu.ace_design.island.map

/**
 * This file is part of the island project
 * @author mosser (23/07/2014, 13:50)
 **/
class ConcreteBuilder extends IslandBuilder {

  override protected val steps: Seq[Process] =
    Seq(IdentifyBorders)

}
