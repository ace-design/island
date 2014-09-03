package eu.ace_design.island.map.processes

import eu.ace_design.island.map._

class AssignElevationTest extends ProcessTestTrait {

  import eu.ace_design.island.map.processes.ElevationFunctions._
  import eu.ace_design.island.map.processes.ElevationMappers._

  "AssignElevationTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    MinimalDistanceToCoast(
      IdentifyCoastLine(
        IdentifyLakesAndOcean(
          AlignVertexWaterBasedOnFaces(
            IdentifyWaterArea(donuts, 30)(IdentifyBorders(m))))))
  }
  override val processUnderTest = AssignElevation(mapper = distance, phi = linear(10))

  "The DistributeElevation process" should {

    val coastline = result.findVerticesWith(Set(IsCoast())) map { p => result.vertexRef(p) }
    import eu.ace_design.island.map.ExistingWaterKind._
    val raw = result.findFacesWith(Set(WaterKind(OCEAN))) flatMap { f => result.cornerRefs(f) + f.center }
    val oceans = raw diff coastline // taking all the vertices involved in oceans, removing coastline
    val lakes = result.findFacesWith(Set(WaterKind(LAKE))) flatMap { f => result.cornerRefs(f) + f.center }

    "not annotate ocean vertices with elevation annotation" in {
      oceans foreach { result.vertexProps.isAnnotatedAs(_, HasForHeight()) must beFalse }
      true must beTrue // glitch to allow implicit conversion (thus compilation). real test is above.
    }
    "give an elevation >= 0 to any land vertex (!ocean, !lake) " in {
      val land = result.vertexRefs diff oceans diff lakes
      land must not be empty
      land foreach { l =>
        result.vertexProps.isAnnotatedAs(l, HasForHeight()) must beTrue
        result.vertexProps.getValue(l, HasForHeight()) must beGreaterThanOrEqualTo(0.0)
      }
      true must beTrue
    }
    "assign elevations to lakes (including centers)" in {
      lakes foreach { result.vertexProps.isAnnotatedAs(_, HasForHeight()) must beTrue }
      lakes must not(beEmpty)
    }
  }

  "The ElevationDistributions library" should {
    val vertices = (0 until 100).toSeq

    "define the linear function" in {
      val result = ElevationDistributions.linear(100)(vertices)
      result foreach { case (k, v) => k.toDouble must beCloseTo(v, 0.0001) }
      result must haveSize(vertices.size)
      result(vertices(0))                  must_== 0.0
      result(vertices(vertices.size - 1))  must_== 99.0
    }

    "define the flat function" in {
      val result = ElevationDistributions.flat(100)(vertices)
      result(vertices(0))                  must_== 0.0
      result(vertices(vertices.size - 1))  must beLessThanOrEqualTo(100.0)
    }

  }

}