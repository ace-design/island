package eu.ace_design.island.map

import eu.ace_design.island.geom.{VertexRegistry,Point}
import org.specs2.mutable._

class PropertiesTest extends SpecificationWithJUnit {

  "PropertiesTest Specifications".title

  "A PropertySet" should {
    val empty = PropertySet()
    val prop = IsWater()
    "be empty when created for the first time" in { empty.size must_== 0 }
    "return the emptySet while using a non existing index" in { empty.get(42) must beEmpty }
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
    "support projection" in {
      val reg = VertexRegistry() + Point(0.0,0.0) + Point(1.0,1.0) + Point(2.0,2.0) + Point(3.0,3.0)
      val pSet = empty + (3 -> IsBorder()) bulkAdd(Set(0,2) -> HasForHeight(100)) bulkAdd(Set(0,1) -> IsWater())
      val projected = pSet.project(reg) _
      projected(Set(HasForHeight(100))) must    contain(Point(0.0,0.0),Point(2.0,2.0)).exactly
      projected(Set(IsBorder()))        must_== Set(Point(3.0,3.0))
      projected(Set(IsWater()))         must    contain(Point(0.0,0.0), Point(1.0,1.0)).exactly
      projected(Set(IsWater(), HasForHeight(100))) must_== Set(Point(0.0,0.0))
    }

  }

}