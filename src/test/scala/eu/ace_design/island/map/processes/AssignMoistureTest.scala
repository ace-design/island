package eu.ace_design.island.map.processes

import eu.ace_design.island.map._

class AssignMoistureTest extends ProcessTestTrait {

  "AssignMoistureTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    GenerateRivers(sources = 2, distance = 0.2)(
      AssignElevation(ElevationFunctions.identity)(
        MinimalDistanceToCoast(
          IdentifyCoastLine(
            IdentifyLakesAndOcean(
              AlignVertexWaterBasedOnFaces(
                IdentifyWaterArea(donuts, 30)(IdentifyBorders(m))))))))
  }
  override val updated = AssignMoisture(MoisturePropagation.order1, MoistureDistribution.identity)(preconditions(entry))

  "The AssignMoisture process" should {

    // vertices tagged as land
    val lands = updated.findVerticesWith(Set(!IsWater())) map { updated.vertexRef }

    // vertices involved in rivers (riverflow is defined on the edges => finding the involved vertices)
    val riverFlow = updated.edgeProps.restrictedTo(RiverFlow())
    val rivers = (riverFlow.keys flatMap { r => val e = updated.edge(r); Seq(e.p1, e.p2) }).toSet

    "Assign a moisture to each vertex identified as land" in {
      lands foreach { updated.vertexProps.getValue(_, HasForMoisture()) must beGreaterThanOrEqualTo(0.0) }
      lands must not(beEmpty)
    }

    "Assign an high moisture level (>=100) for vertices involved in rivers" in {
      rivers foreach { updated.vertexProps.getValue(_, HasForMoisture()) must beGreaterThanOrEqualTo(100.0) }
      rivers must not(beEmpty)
    }

  }

  "The MoistureDistribution function library" should {

    val data = (for(i <- 0 until 100) yield i -> 2.0*i).toMap

    "expose the identity function" in { MoistureDistribution.identity(data) must_== data }

  }

  "The MoisturePropagation function library" should {

    val fSqrt = MoisturePropagation(MoisturePropagation.orderSqrt) _
    val f1 = MoisturePropagation(MoisturePropagation.order1) _
    val f2 = MoisturePropagation(MoisturePropagation.order2) _
    val f3 = MoisturePropagation(MoisturePropagation.order3) _
    val f4 = MoisturePropagation(MoisturePropagation.order4) _

    "assign 100 for a point identified as fresh water (distance = 0)" in {
      fSqrt(0) must_== 100; f1(0) must_== 100; f2(0) must_== 100; f3(0) must_== 100; f4(0) must_== 100
    }

    "assign 0 to points located at 50+ the source" in {
      fSqrt(50) must_== 0; f1(50) must_== 0; f2(50) must_== 0; f3(50) must_== 0; f4(50) must_== 0
    }

    "reject negative distance" in {
      fSqrt(-1) must throwAn[IllegalArgumentException]
      f1(-1)    must throwAn[IllegalArgumentException]
      f2(-1)    must throwAn[IllegalArgumentException]
      f3(-1)    must throwAn[IllegalArgumentException]
      f4(-1)    must throwAn[IllegalArgumentException]
    }
  }

}