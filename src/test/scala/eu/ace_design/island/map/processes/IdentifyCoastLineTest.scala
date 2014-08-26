package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{IsCoast, IsWater, DonutShape, IslandMap}
import org.specs2.mutable._

class IdentifyCoastLineTest extends SpecificationWithJUnit {

  "IdentifyCoastLineTest Specifications".title

  "The IdentifyCoastLine process" should {
    val preconditions : IslandMap => IslandMap = { m =>
      val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
      IdentifyLakesAndOcean(AlignVertexWaterBasedOnFaces(IdentifyWaterArea(donuts,30)(IdentifyBorders(m))))
    }
    val updated = IdentifyCoastLine(preconditions(entry))

    "annotate land faces with an IsCoast tag" in {
      val land = updated.findFacesWith(Set(!IsWater()))
      val coast = updated.findFacesWith(Set(IsCoast()))
      coast must not(beEmpty)
      (coast & land) must_== coast
    }
    "Annotate land vertices with the IsCoast tag" in {
      val coast = updated.findVerticesWith(Set(IsCoast()))
      val land = updated.findVerticesWith(Set(!IsWater()))
      coast must not(beEmpty)
      (coast & land) must_== coast
    }
  }

}