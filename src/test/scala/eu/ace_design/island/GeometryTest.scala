package eu.ace_design.island

import eu.ace_design.island.geom.{Edge, Point, VertexRegistry}
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GeometryTest extends SpecificationWithJUnit {

  "Geometry Specifications".title

  /**
   * Data used for test purpose 
   */

  val p1 = Point(0.0, 0.1); val p2 = Point(2.5, 4.9)
  val p3 = Point(4.2, 8.9) ; val p4 = Point(7.4, 9.9); val p5 = Point(1.8, 9.5)

  val regP =  VertexRegistry() + p1 + p2
  val regPP = VertexRegistry() + p3 + p4 + p5

  val e1 = Edge(regP(p1).get, regP(p1).get)


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
  

  "A VertexRegistry" should {
    "be empty when initialized" in { VertexRegistry().size must_== 0 }
    "support functional adding" in {
      val init = VertexRegistry()
      val regP = init + Point(1.0,1.3)
      init.size must_== 0
      regP.size must_== 1
    }
    "support adding as a sequential operator" in {
      regP(0) must_== p1
      regP(1) must_== p2
      regP(2) must throwA[NoSuchElementException]
    }
    "support looking for a given vertex" in {
      regP(p1) must beSome(0)
      regP(p2) must beSome(1)
      regP(Point(0.0,0.0)) must beNone
    }
    "support the + operator" in {
      val sum = regP + regPP
      sum.size must_== 5
    }
    "change index of its right parameter" in {
      val sum = regP + regPP
      sum(p1) must beSome(0)
      sum(p2) must beSome(1)
      sum(p3) must beSome( (i: Int) => i != sum(p4).get && i != sum(p5).get )
      sum(p4) must beSome( (i: Int) => i != sum(p3).get && i != sum(p5).get )
      sum(p5) must beSome( (i: Int) => i != sum(p3).get && i != sum(p4).get )
    }
    "do not introduce duplicates while adding a point into a registry " in {
      val nReg = regP + p1
      nReg(p1) must_== regP(p1)
      nReg.size must_== regP.size
    }
    "do not introduce duplicate points while merging two registries" in {
      val sum = regP + (VertexRegistry() + p1)
      sum.size must_== regP.size
      sum(p1) must beSome(0)
    }
  }

}