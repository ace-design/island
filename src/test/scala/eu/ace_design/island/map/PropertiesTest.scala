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
      val pSet = empty + (0 -> prop) + (1 -> !prop) + (2 -> DistanceToCoast(0.3))
      pSet.size must_== 3
      // positive checking
      pSet.check(0, prop)  must beTrue
      pSet.check(0, !prop) must beFalse
      // negative checking
      pSet.check(1, prop)  must beFalse
      pSet.check(1, !prop) must beTrue
      // no property => negative answer
      pSet.check(2, prop) must beFalse
      pSet.check(2, !prop) must beFalse
      // non-boolean properties
      pSet.check(2, DistanceToCoast(0.3)) must beTrue
      pSet.check(2, DistanceToCoast(3.4)) must beFalse
      pSet.check(2, HasForHeight(0.0)) must beFalse
    }
    "support annotation checking (agnostic of value)" in {
      val pSet = empty + (0 -> prop) + (1 -> DistanceToCoast(0.3))
      pSet.isAnnotatedAs(0, prop) must beTrue
      pSet.isAnnotatedAs(1, prop) must beFalse
      pSet.isAnnotatedAs(0, DistanceToCoast()) must beFalse
      pSet.isAnnotatedAs(1, DistanceToCoast()) must beTrue
      pSet.isAnnotatedAs(2, prop) must beFalse
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
    "Support restriction" in {
      val pSet = empty +
                 (1 -> DistanceToCoast(3.3)) + (1-> HasForHeight(3.4)) +
                 (2 -> IsBorder()) +
                 (3 -> DistanceToCoast(2))

      val waters    = pSet restrictedTo IsWater()
      waters must beEmpty

      val borders   = pSet restrictedTo IsBorder()
      borders must haveSize(1); borders must havePair(2 -> true)

      val heights   = pSet restrictedTo HasForHeight()
      heights must haveSize(1); heights must havePair(1 -> 3.4)

      val distances = pSet restrictedTo DistanceToCoast()
      distances must haveSize(2); distances must havePairs(1 -> 3.3, 3 -> 2)
    }
    "support value retrieval for a given reference" in {
      val pSet = empty + (1 -> DistanceToCoast(3.3)) + (1-> HasForHeight(3.4))
      pSet.getValue(0, IsWater()) must throwAn[IllegalArgumentException]
      pSet.getValue(1, IsWater()) must throwAn[IllegalArgumentException]
      pSet.getValue(1, DistanceToCoast()) must_== 3.3
      pSet.getValue(1, HasForHeight()) must_== 3.4
    }

  }

}