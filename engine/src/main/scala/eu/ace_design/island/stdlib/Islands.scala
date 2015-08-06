package eu.ace_design.island.stdlib

import eu.ace_design.island.dsl.DiSLand
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.map.processes.AssignPitch

/**
 * This file is a library of pre-existing islands.
 **/
object Islands extends DiSLand {

  /**
   * A sample with inner lakes, small and quick to generate
   */
  lazy val default: IslandMap = {
    createIsland shapedAs radial(factor = 1.09) usingSeed 0x24F84E32B99D3CF5L
  }

  /**
   * Shaped as a tore, with a large inner lake in the middle of the island (e.g., a crater lake)
   */
  lazy val donuts: IslandMap = {
    createIsland shapedAs donut(80.percent, 20.percent) withSize 400 having 1200.faces builtWith Seq(
      plateau(23), flowing(rivers = 15, distance = 0.2), withMoisture(soils.normal, distance = 400),
      AssignPitch, usingBiomes()
    )
  }

  /**
   * An island shape as an ellipsis, inspired by the Ile de la Tortue in Haiti
   * Ref: http://en.wikipedia.org/wiki/Tortuga_Island,_Haiti
   */
  lazy val tortuga: IslandMap =  {
    createIsland shapedAs ellipsis(x = 95.percent,
                                   y = 40.percent,
                                   theta = 20) builtWith Seq(
      plateau(45), flowing(rivers = 30, distance = 60.percent),
      withMoisture(soils.wet, distance = 100),
      AssignPitch, usingBiomes(WhittakerDiagrams.caribbean)
    ) usingSeed 0x24F84E32B98D3CF5L withSize 1600 having 3000.faces
  }

  /**
   * This island is inspired by the Realm of Oz, a squared country surrounded by an impassable desert (here the ocean).
   */
  lazy val ozRealm: IslandMap = {
    createIsland shapedAs oz() builtWith Seq(
      plateau(100), flowing(rivers = 45, distance = 0.3), withMoisture(soils.normal, distance = 400),
      AssignPitch, usingBiomes() ) usingSeed 0x5DA96217A1606058L
  }

  /**
   * A nordic island (imaginary), shaped as a butterfly
   */
  lazy val sommerfugloya: IslandMap = {
    createIsland shapedAs radial(factor = 1.26) builtWith Seq(
      flatDistribution(100), flowing(rivers = 30, distance = 0.4), withMoisture(soils.wet, distance = 200),
      AssignPitch, usingBiomes(WhittakerDiagrams.nordic)
    ) usingSeed 0x9AC771d247f74037L withSize 2000 having 4096.faces
  }

}
