package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{WaterKind, IsWater, DonutShape, IslandMap}
import org.specs2.mutable._


class IdentifyLakesAndOceansTest extends SpecificationWithJUnit {

  "IdentifyLakesAndOceansTest Specifications".title

  "The IdentifyLakesAndOcean process" should {
    import eu.ace_design.island.map.ExistingWaterKind._
    // preconditions:  only works on faces => neglect AlignVertex...  Requires borders and IdentifyWaterArea
    val preconditions : IslandMap => IslandMap = { m =>
      val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
      IdentifyWaterArea(shape = donuts, threshold = 30)(IdentifyBorders(m))
    }
    val updated = IdentifyLakesAndOcean(preconditions(entry))
    val waters = updated.findFacesWith(Set(IsWater()))
    val oceans = updated.findFacesWith(Set(WaterKind(OCEAN)))
    val lakes = updated.findFacesWith(Set(WaterKind(LAKE)))

    "annotate all the water faces with WaterKind properties" in { lakes ++ oceans must_== waters }
    "identify the external ocean" in { oceans.size must be greaterThan 0 }
    "identify the central lake of this donuts-shaped island" in { lakes.size  must be greaterThan 0 }
  }

}