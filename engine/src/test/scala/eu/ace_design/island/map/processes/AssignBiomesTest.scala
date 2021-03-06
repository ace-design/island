package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import eu.ace_design.island.stdlib.{WhittakerDiagrams, Biomes}
import org.specs2.mutable._

import Biomes._

class AssignBiomesTest extends ProcessTestTrait {

  "AssignBiomesTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    val moisturizer = AssignMoisture(MoisturePropagation.linear(100), aquifers = 0)
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    moisturizer(
      GenerateRivers(sources = 2, distance = 0.2)(
        DistributeElevation(elevator = ElevationDistributions.linear(100))(
          MinimalDistanceToCoast(
            IdentifyCoastLine(
              IdentifyLakesAndOcean(
                AlignVertexWaterBasedOnFaces(
                  IdentifyWaterArea(donuts, 30)(IdentifyBorders(m)))))))))
  }
  override val processUnderTest = AssignBiomes()
  //override val result = AssignBiomes()(preconditions(entry))

  private val BiomeProperty = HasForBiome(SNOW) // SNOW for type compliance, working with the type for set restriction

  "The AssignBiomes process" should {

    "assign a biome to each face defined in the map" in {
      val biomes = result.faceProps.restrictedTo(BiomeProperty)
      biomes.keys must_== result.faceRefs
    }

    "identify oceans faces with the 'OCEAN' biome" in {
      val oceanKinds = result.findFacesWith(Set(WaterKind(ExistingWaterKind.OCEAN))) map { result.faceRef }
      val oceanBiomes = result.findFacesWith(Set(HasForBiome(OCEAN))) map { result.faceRef }
      oceanBiomes must_== oceanKinds
    }

    "identify lake faces as LAKEs or GLACIERs" in {
      val lakeKinds = result.findFacesWith(Set(WaterKind(ExistingWaterKind.LAKE))) map { result.faceRef }
      val lakeBiomes = result.findFacesWith(Set(HasForBiome(LAKE))) map { result.faceRef }
      val glacierBiomes = result.findFacesWith(Set(HasForBiome(GLACIER))) map { result.faceRef }
      lakeBiomes ++ glacierBiomes must_== lakeKinds
    }

    "identify land faces as anything that is not a LAKE, an OCEAN or a GLACIER" in {
      val landFaces = result.findFacesWith(Set(!IsWater())) map { result.faceRef }
      val rejected = Set(OCEAN, GLACIER, LAKE)
      landFaces foreach { l => rejected.contains(result.faceProps.getValue(l, BiomeProperty)) must beFalse }
      true must beTrue
    }

  }
}


