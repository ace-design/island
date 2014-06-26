package eu.ace_design.island

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GeometryTest extends SpecificationWithJUnit {

  "Geometry Specifications".title


  "A Point" should {
    val x = 2.0; val y = 5.0;  val p = Point(x,y)

    "hold an X coordinate" in { p.x must_== x }
    "hold an Y coordinate" in { p.y must_== y }
  }

  "A VerticeRegistry" should {

    val reg = new VertexRegistry()
    val p1 = Point(0.0, 0.1); val p2 = Point(2.5, 4.9)
    val regP = reg + p1 + p2

    "be empty when initialized" in { reg.size must_== 0 }
    "support functional adding" in {
      val regP = reg + Point(1.0,1.3)
      reg.size must_== 0
      regP.size must_== 1
    }
    "support adding as a sequential operator" in {
      regP(0) must_== p1
      regP(1) must_== p2
      regP(2) must throwA[NoSuchElementException]
    }
    "support looking for vertice" in {
      regP(p1) must beSome(0)
      regP(p2) must beSome(1)
      regP(Point(0.0,0.0)) must beNone
    }

  }

}