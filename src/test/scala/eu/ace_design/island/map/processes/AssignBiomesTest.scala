package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import org.specs2.mutable._

import eu.ace_design.island.map.ExistingBiomes._

class AssignBiomesTest extends ProcessTestTrait {

  "AssignBiomesTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    val moisturizer = AssignMoisture(MoisturePropagation.linear(100), aquifers = 0)
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    moisturizer(
      GenerateRivers(sources = 2, distance = 0.2)(
        AssignElevation(ElevationFunctions.identity)(
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

class WhittakerDiagramsTest extends SpecificationWithJUnit {

  import WhittakerDiagrams.complete

  "WhittakerDiagramsTest Specifications".title

  "The complete diagram" should {

    "return a glacier when a lake is located above 1300m" in {
      complete freshWater 129.9 must_== LAKE
      complete freshWater 130   must_== GLACIER
    }

    "reject non-relevant values" in {
      complete(moisture = -1, elevation = 20) must throwAn[IllegalArgumentException]
      complete(moisture = 101, elevation = 20) must throwAn[IllegalArgumentException]
      complete(moisture = 50, elevation = -1) must throwAn[IllegalArgumentException]
    }

    // TODO test the distribution of the 14 biomes
  }

}
