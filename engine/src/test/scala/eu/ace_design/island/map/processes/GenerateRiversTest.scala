package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{RiverFlow, DiskShape, IslandMap}

class GenerateRiversTest extends ProcessTestTrait {

  "GenerateRiversTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    val disk = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8)
    DistributeElevation(elevator = ElevationDistributions.linear(100))(
      MinimalDistanceToCoast(
        IdentifyCoastLine(
          IdentifyLakesAndOcean(
            AlignVertexWaterBasedOnFaces(
              IdentifyWaterArea(disk, 30)(IdentifyBorders(m)))))))
  }
  override val processUnderTest = GenerateRivers(sources = 1)

  "The GenerateRivers process " should {

    "reject a negative number of sources" in {
      GenerateRivers(sources = -1) must throwAn[IllegalArgumentException]
    }

    "reject a non-normalized distance" in { // normalized means in ]0,1]
      GenerateRivers(distance = 0)  must throwAn[IllegalArgumentException]
      GenerateRivers(distance = -1) must throwAn[IllegalArgumentException]
      GenerateRivers(distance = 2)  must throwAn[IllegalArgumentException]
    }

    "annotate edges with river flow" in {
      val rivers = result.findEdgesWith(Set(RiverFlow()))
      rivers must not(beEmpty)
    }

  }


}