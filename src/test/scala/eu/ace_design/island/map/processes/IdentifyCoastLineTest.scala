package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{IsCoast, IsWater, DonutShape, IslandMap}


class IdentifyCoastLineTest extends ProcessTestTrait {

  "IdentifyCoastLineTest Specifications".title

  override val preconditions : IslandMap => IslandMap = { m =>
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    IdentifyLakesAndOcean(AlignVertexWaterBasedOnFaces(IdentifyWaterArea(donuts,30)(IdentifyBorders(m))))
  }
  override val result = IdentifyCoastLine(preconditions(entry))

  "The IdentifyCoastLine process" should {

    "annotate land faces with an IsCoast tag" in {
      val land = result.findFacesWith(Set(!IsWater()))
      val coast = result.findFacesWith(Set(IsCoast()))
      coast must not(beEmpty)
      (coast & land) must_== coast
    }
    "Annotate land vertices with the IsCoast tag" in {
      val coast = result.findVerticesWith(Set(IsCoast()))
      val land = result.findVerticesWith(Set(!IsWater()))
      coast must not(beEmpty)
      (coast & land) must_== coast
    }
  }

}