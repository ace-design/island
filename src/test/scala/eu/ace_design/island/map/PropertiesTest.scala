package eu.ace_design.island.map

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropertiesTest extends SpecificationWithJUnit {

  "PropertiesTest Specifications".title

  "A PropertySet" should {
    val empty = PropertySet()
    val prop = IsWater()
    "be empty when initialized" in { empty.size must_== 0 }
    "support indexing of properties" in {
      val pSet = empty + (1 -> prop)
      pSet.size must_== 1
      pSet.get(1) must_== Set(prop)
    }
    "support checking" in {
      val pSet = empty + (0 -> prop) + (1 -> !prop)
      pSet.size must_== 2
      pSet.check(0, prop)  must beTrue
      pSet.check(0, !prop) must beFalse
      pSet.check(1, prop)  must beFalse
      pSet.check(1, !prop) must beTrue
    }
    "support property update" in {
      val pSet = empty + (0 -> prop)
      val pSetP = pSet + (0 -> !prop)
      pSet.size must_== 1
      pSetP.get(0) must_== Set(!prop)

    }
    "support multiple properties at the same index" in {
      val h = HasForHeight()
      val pSet = empty + (0 -> prop) + (0 -> h)
      pSet.size must_== 1
      pSet.get(0) must_== Set(prop, h)
      pSet.check(0, prop) must beTrue
      pSet.check(0, h)    must beTrue
    }
    "support mass assignments" in {
      val targets = Set(1,2,3,4,5)
      val pSet = empty  + (1 -> HasForHeight()) bulkAdd (targets -> prop)
      pSet.size must_== targets.size
      pSet.get(1) must_== Set(prop, HasForHeight())
    }


  }

}