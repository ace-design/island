package eu.ace_design.island.geom

import org.specs2.mutable._

class RegistriesTest extends SpecificationWithJUnit {

  "RegistriesTest Specifications".title

  /**
   * Data used for test purpose
   */

  val p1 = Point(0.0, 0.1); val p2 = Point(2.5, 4.9)
  val p3 = Point(4.2, 8.9) ; val p4 = Point(7.4, 9.9); val p5 = Point(1.8, 9.5)

  val vReg =  VertexRegistry() + p1 + p2
  val vRegP = VertexRegistry() + p3 + p4 + p5

  val e1 = Edge(vRegP(p3).get, vRegP(p4).get)
  val e2 = Edge(vRegP(p4).get, vRegP(p5).get)
  val e3 = Edge(vRegP(p5).get, vRegP(p3).get)
  val e4 = Edge(30,40)
  val eReg = EdgeRegistry() + e1 + e2 + e3
  val eRegP = EdgeRegistry() + e4

  "A VertexRegistry" should {
    "be empty when initialized" in { VertexRegistry().size must_== 0 }
    "support functional adding" in {
      val init = VertexRegistry()
      val regP = init + Point(1.0,1.3)
      init.size must_== 0
      regP.size must_== 1
    }
    "support adding as a sequential operator" in {
      vReg(0) must_== p1
      vReg(1) must_== p2
      vReg(2) must throwA[NoSuchElementException]
    }
    "support looking for a given vertex" in {
      vReg(p1) must beSome(0)
      vReg(p2) must beSome(1)
      vReg(Point(0.0,0.0)) must beNone
    }
    "support the + operator" in {
      val sum = vReg + vRegP
      sum.size must_== 5
    }
    "change index of its right parameter" in {
      val sum = vReg + vRegP
      sum(p1) must beSome(0)
      sum(p2) must beSome(1)
      sum(p3) must beSome( (i: Int) => i != sum(p4).get && i != sum(p5).get )
      sum(p4) must beSome( (i: Int) => i != sum(p3).get && i != sum(p5).get )
      sum(p5) must beSome( (i: Int) => i != sum(p3).get && i != sum(p4).get )
    }
    "not introduce duplicates while adding a point into a registry " in {
      val nReg = vReg + p1
      nReg(p1) must_== vReg(p1)
      nReg.size must_== vReg.size
    }
    "not introduce duplicate points while merging two registries" in {
      val sum = vReg + (VertexRegistry() + p1)
      sum.size must_== vReg.size
      sum(p1) must beSome(0)
    }
  }

  "An EdgeRegistry" should {
    "be empty when initialized" in { EdgeRegistry().size must_== 0 }
    "support functional adding" in {
      val init = EdgeRegistry()
      val regP = init + Edge(0,1)
      init.size must_== 0
      regP.size must_== 1
      }
    "support adding as a sequential operator" in {
      eReg(0) must_== e1
      eReg(1) must_== e2
      eReg(2) must_== e3
      eReg(3) must throwAn[NoSuchElementException]
    }
    "support looking for a given edge" in {
      eReg(e1) must beSome(0)
      eReg(e2) must beSome(1)
      eReg(Edge(10,11)) must beNone
    }
    "support the + operator" in {
      val sum = eReg + eRegP
      sum.size must_== 4
    }
    "change index of its right parameter" in {
      val sum = eReg + eRegP
      sum(e1) must beSome(0)
      sum(e2) must beSome(1)
      sum(e3) must beSome(2)
      eRegP(e4)  must beSome(0)
      sum(e4) must beSome(3)
    }
    "not introduce duplicates while adding an edge into a registry" in {
      val nReg = eReg + e1
      nReg(e1) must_== nReg(e1)
      nReg.size must_== eReg.size
    }
    "not introduce duplicate edges while merging two registries" in {
      val sum = eReg + (EdgeRegistry() + e1)
      sum.size must_== eReg.size
      sum(e1) must beSome(0)
    }
  }
}