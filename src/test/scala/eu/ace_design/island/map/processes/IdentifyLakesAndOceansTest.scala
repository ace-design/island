package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{WaterKind, IsWater, DonutShape, IslandMap}
import eu.ace_design.island.map.ExistingWaterKind._


class IdentifyLakesAndOceansTest extends ProcessTestTrait {

  "IdentifyLakesAndOceansTest Specifications".title

  override val preconditions : IslandMap => IslandMap = { m =>
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    IdentifyWaterArea(shape = donuts, threshold = 30)(IdentifyBorders(m))
  }
  override val result = IdentifyLakesAndOcean(preconditions(entry))

  "The IdentifyLakesAndOcean process" should {

    // preconditions:  only works on faces => neglect AlignVertex...  Requires borders and IdentifyWaterArea

    val waters = result.findFacesWith(Set(IsWater()))
    val oceans = result.findFacesWith(Set(WaterKind(OCEAN)))
    val lakes = result.findFacesWith(Set(WaterKind(LAKE)))

    "annotate all the water faces with WaterKind properties" in { lakes ++ oceans must_== waters }
    "identify the external ocean" in { oceans.size must be greaterThan 0 }
    "identify the central lake of this donuts-shaped island" in { lakes.size  must be greaterThan 0 }
  }

}