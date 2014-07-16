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

  "A Mesh" should {
    val mesh = Mesh()
    "not have size by default" in { mesh.size must beNone }
    "support clipping" in {
      val origin = mesh.size
      val mP = mesh clip 200
      mP.size must beSome(200)
      mesh.size must_== origin

    }

  }


}