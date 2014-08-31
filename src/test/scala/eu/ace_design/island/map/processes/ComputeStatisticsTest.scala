package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{DonutShape, IslandMap}

class ComputeStatisticsTest extends ProcessTestTrait {

  "AssignBiomesTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    ComputeArea(
      AssignBiomes()(
        AssignMoisture(MoisturePropagation.linear(100), aquifers = 0)(
          GenerateRivers(sources = 2, distance = 0.2)(
            AssignElevation(ElevationFunctions.identity)(
              MinimalDistanceToCoast(
                IdentifyCoastLine(
                  IdentifyLakesAndOcean(
                    AlignVertexWaterBasedOnFaces(
                      IdentifyWaterArea(DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2), 30)(
                        IdentifyBorders(m)))))))))))
  }
  override val processUnderTest = ComputeStatistics

  "The compute statistics process" should {

    "Initialise the stats data slot in a map" in {
      entry.stats  must beNone
      result.stats must beSome
    }

  }

}
