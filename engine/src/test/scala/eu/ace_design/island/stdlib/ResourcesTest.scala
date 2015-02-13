package eu.ace_design.island.stdlib

import org.specs2.mutable.SpecificationWithJUnit

class ResourcesTest extends SpecificationWithJUnit {

  "ExistingResources Specifications".title

  "The Biome -> Resource mapper" should {
    "map each declared biome to a resource" in {
      Biomes.values foreach { biome => biome() must not(throwAn[Exception]) }
      true must beTrue
    }
  }
}