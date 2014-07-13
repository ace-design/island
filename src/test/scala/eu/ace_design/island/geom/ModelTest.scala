package eu.ace_design.island.geom

import org.specs2.mutable._

class ModelTest extends SpecificationWithJUnit {

  "Geometry Model Specifications".title

  "A Point" should {
    val x = 2.0; val y = 5.0;  val p = Point(x,y)
    "hold an X coordinate" in { p.x must_== x }
    "hold an Y coordinate" in { p.y must_== y }
  }

  "An Edge" should {
    "be constructed in any order (non-directional)" in {
      Edge(1,2) must_== Edge(2,1)
    }
  }

}