package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import org.specs2.mutable._

class MinimalDistanceToCoastTest extends ProcessTestTrait {

  "MinimalDistanceToCoastTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    IdentifyCoastLine(IdentifyLakesAndOcean(AlignVertexWaterBasedOnFaces(IdentifyWaterArea(donuts, 30)(IdentifyBorders(m)))))
  }
  override val processUnderTest = MinimalDistanceToCoast

  "The DistanceToCoast process" should {

    "consider coastal vertices as lowest distance (0)" in {
      val coast = result.findVerticesWith(Set(IsCoast())) map { p => result.vertexRef(p) }
      coast foreach { result.vertexProps.getValue(_, DistanceToCoast()) must_== 0 }
      true must beTrue
    }

    "assign a distance to each land vertices" in {
      val land = result.findVerticesWith(Set(!IsWater())) map { p => result.vertexRef(p)  }
      land foreach { result.vertexProps.getValue(_, DistanceToCoast()) must be greaterThanOrEqualTo(0.0) }
      true must beTrue
    }
  }

}