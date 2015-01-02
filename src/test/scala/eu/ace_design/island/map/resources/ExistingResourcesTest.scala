package eu.ace_design.island.map.resources

import eu.ace_design.island.stdlib.{ExistingBiomes, BiomeToResource}
import org.specs2.mutable.SpecificationWithJUnit

class ExistingResourcesTest extends SpecificationWithJUnit {

  "ExistingResources Specifications".title

  "The Biome -> Resource mapper" should {
    "map each declared biome to a resource" in {
      ExistingBiomes.values foreach { BiomeToResource(_) must not(throwAn[Exception]) }
      true must beTrue
    }
  }
}