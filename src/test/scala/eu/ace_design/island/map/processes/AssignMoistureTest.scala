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
  override val processUnderTest = AssignMoisture(MoisturePropagation.linear(100), aquifers = 0)


  "The AssignMoisture process" should {

    // vertices tagged as land
    val lands = result.findVerticesWith(Set(!IsWater())) map {
      result.vertexRef
    }

    // vertices involved in rivers (riverflow is defined on the edges => finding the involved vertices)
    val riverFlow = result.edgeProps.restrictedTo(RiverFlow())
    val rivers = (riverFlow.keys flatMap { r => val e = result.edge(r); Seq(e.p1, e.p2)}).toSet

    "assign a moisture to each vertex identified as land" in {
      lands foreach {
        result.vertexProps.getValue(_, HasForMoisture()) must beGreaterThanOrEqualTo(0.0)
      }
      lands must not(beEmpty)
    }

    "assign an high moisture level (100) for vertices involved in rivers" in {
      rivers foreach { result.vertexProps.getValue(_, HasForMoisture()) must_== 100.0 }
      rivers must not(beEmpty)
    }

    "assign a moisture to each land face" in {
      val withMoisture = result.faceProps.restrictedTo(HasForMoisture()).keys.toSet
      val lands = result.findFacesWith(Set(!IsWater())) map { result.faceRef }
      withMoisture must_== lands
    }

  }

  "The MoisturePropagation function library" should {
    val dist = 300
    val dry = MoisturePropagation.dry(dist) _
    val wet = MoisturePropagation.wet(dist) _

    "support a 'dry' moisture propagation" in {
      val f1 = dry(1)
      f1(0) must_== MoisturePropagation.MAX_MOISTURE
      f1(dist.toDouble) must_== 0

      val f2 = dry(2)
      f2(0) must_== MoisturePropagation.MAX_MOISTURE
      f2(dist.toDouble) must_== 0

      val f3 = dry(3)
      f3(0) must_== MoisturePropagation.MAX_MOISTURE
      f3(dist.toDouble) must_== 0

      f1(150) must beLessThan(50.0)
      f2(150) must beLessThan(f1(150))
      f3(150) must beLessThan(f2(150))

      f1(400) must_== 0.0
      f2(400) must_== 0.0
      f3(400) must_== 0.0
    }

    "support a 'wet' moisture propagation" in {
      val f1 = wet(1)
      f1(0) must_== MoisturePropagation.MAX_MOISTURE
      f1(dist.toDouble) must_== 0

      val f2 = wet(2)
      f2(0) must_== MoisturePropagation.MAX_MOISTURE
      f2(dist.toDouble) must_== 0

      val f3 = wet(3)
      f3(0) must_== MoisturePropagation.MAX_MOISTURE
      f3(dist.toDouble) must_== 0

      f1(150) must beGreaterThan(50.0)
      f2(150) must beGreaterThan(f1(150))
      f3(150) must beGreaterThan(f2(150))

      f1(400) must_== 0.0
      f2(400) must_== 0.0
      f3(400) must_== 0.0
    }
  }
}