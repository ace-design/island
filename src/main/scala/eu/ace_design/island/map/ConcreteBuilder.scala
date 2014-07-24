package eu.ace_design.island.map

/**
 * This file is part of the island project
 * @author mosser (23/07/2014, 13:50)
 **/
class ConcreteBuilder(override val size: Int) extends IslandBuilder {

  final val ISLAND_SHAPE = CircularShape(size, size.toDouble/2 * 0.85)
  final val WATER_THRESHOLD = 30  // a face is a water one if 30% of its corners are in a water area

  override protected val steps: Seq[Process] =
    Seq(IdentifyBorders,
        IdentifyWaterArea(ISLAND_SHAPE, WATER_THRESHOLD))

}
