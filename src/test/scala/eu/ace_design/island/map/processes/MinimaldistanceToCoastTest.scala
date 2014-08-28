package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import org.specs2.mutable._

class MinimalDistanceToCoastTest extends ProcessTestTrait {

  "MinimalDistanceToCoastTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    IdentifyCoastLine(IdentifyLakesAndOcean(AlignVertexWaterBasedOnFaces(IdentifyWaterArea(donuts, 30)(IdentifyBorders(m)))))
  }
  override val updated = MinimalDistanceToCoast(preconditions(entry))

  "The DistanceToCoast process" should {

    "consider coastal vertices as lowest distance (0)" in {
      val coast = updated.findVerticesWith(Set(IsCoast())) map { p => updated.vertexRef(p) }
      coast foreach { updated.vertexProps.getValue(_, DistanceToCoast()) must_== 0 }
      true must beTrue
    }

    "assign a distance to each land vertices" in {
      val land = updated.findVerticesWith(Set(!IsWater())) map { p => updated.vertexRef(p)  }
      land foreach { updated.vertexProps.getValue(_, DistanceToCoast()) must be greaterThanOrEqualTo(0.0) }
      true must beTrue
    }
  }

}