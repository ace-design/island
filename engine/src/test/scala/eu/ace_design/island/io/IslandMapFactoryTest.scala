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

    val json = IslandMapFactory(Islands.default)

    "support JSON serialization as a JSON object" in {
      json must beAnInstanceOf[JSONObject]
    }

    "be a bidirectional transformation" in {
      IslandMapFactory(json) must_== Islands.default
    }

    "support serialization of optional elements" in {
      val downgraded = Islands.default.copy(uuid = None, stats = None)
      IslandMapFactory(IslandMapFactory(downgraded)) must_== downgraded
    }

  }
}