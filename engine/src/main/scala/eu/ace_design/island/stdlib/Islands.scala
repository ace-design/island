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


  /*******************************************************
   ** Reference islands used by the evolution dashboard **
   *******************************************************/

  lazy val aloneInTheOcean: IslandMap = {
    createIsland shapedAs ellipsis(x=5.percent, y=5.percent, theta=10) withSize 2000 having 4096.faces  builtWith Seq(
      plateau(1), flowing(rivers = 1, distance = 90.percent), withMoisture(soils.wet, distance = 100),
      AssignPitch, usingBiomes(WhittakerDiagrams.caribbean)) usingSeed  0xE19CE5A87D14FBEEL
  }

  lazy val largeIsland: IslandMap = {
    createIsland shapedAs ellipsis(x=90.percent, y=75.percent, theta=10) withSize 2000 having 4096.faces  builtWith Seq(
      plateau(45), flowing(rivers = 60, distance = 90.percent), withMoisture(soils.normal, distance = 200),
      AssignPitch, usingBiomes(WhittakerDiagrams.caribbean)) usingSeed  0xF93741FB0DB4796FL
  }

  lazy val easyIsland: IslandMap = {
    createIsland shapedAs ellipsis(x=50.percent, y=30.percent, theta=10) withSize 2000 having 4096.faces  builtWith Seq(
      plateau(45), flowing(rivers = 30, distance = 40.percent), withMoisture(soils.wet, distance = 800),
      AssignPitch, usingBiomes(WhittakerDiagrams.caribbean)) usingSeed 0xC8532AA8FE8211D9L
  }

  lazy val smallForest: IslandMap = {
    createIsland shapedAs ellipsis(x=90.percent, y=70.percent, theta=30) withSize 2000 having 4096.faces  builtWith Seq(
      plateau(50), flowing(rivers = 10, distance = 40.percent), withMoisture(soils.dry, distance = 100),
      AssignPitch, usingBiomes(WhittakerDiagrams.caribbean)) usingSeed 0x5A5E718FB090236DL
  }

  lazy val complete: IslandMap = {
    createIsland shapedAs radial(factor=1.19) withSize 2000 having 4096.faces  builtWith Seq(
      plateau(50), flowing(rivers = 30, distance = 50.percent), withMoisture(soils.normal, distance = 100),
      AssignPitch, usingBiomes(WhittakerDiagrams.complete)) usingSeed 0xD15183E20DDC0219L
  }

  lazy val forests: IslandMap = {
    createIsland shapedAs ellipsis(x=90.percent,y=80.percent) withSize 2000 having 4096.faces builtWith Seq(
      plateau(50), flowing(rivers = 0, distance = 50.percent), withMoisture(soils.normal, distance = 300),
      AssignPitch, usingBiomes(WhittakerDiagrams.complete)) usingSeed 0xCF2964929AEC05C0L
  }


}
