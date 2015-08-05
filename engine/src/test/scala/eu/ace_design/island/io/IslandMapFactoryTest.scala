package eu.ace_design.island.io

import eu.ace_design.island.stdlib.Islands
import org.json.JSONObject
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IslandMapFactoryTest extends SpecificationWithJUnit {

  "IslandMapFactory Specifications".title

  "The Island Map Factory" should {

    "support JSON serialization" in {
      IslandMapFactory(Islands.default) must beAnInstanceOf[JSONObject]
    }

    "be a bidirectional transformation" in {
      val json = IslandMapFactory(Islands.default)
      val map  = IslandMapFactory(json)
      map must_== Islands.default
    }
  }
}