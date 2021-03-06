package eu.ace_design.island.geom

import org.specs2.mutable._
// TODO Refactor this class to avoid test duplication, this is quite annoying
class RegistriesTest extends SpecificationWithJUnit {

  "RegistriesTest Specifications".title

  /********************************
   ** Data used for test purpose **
   ********************************/

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

  val f1 = Face(0,Seq(1,2,3))
  val f2 = Face(10,Seq(11,12,13))
  val f3 = Face(100, Seq(101,102,103))
  val fReg = FaceRegistry() + f1 + f2
  val fRegP = FaceRegistry() + f3

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
    "return its references" in {
      vReg.references must contain(0,1).exactly
      vRegP.references must contain(0,1,2).exactly
    }
    "return its registered vertices" in {
      vReg.values must contain(p1,p2).exactly
      vRegP.values must contain(p3,p4,p5).exactly
    }
    "support querying" in {
      val refs = vReg queryReferences( _.x == 0 )
      refs must_== Set(0)
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
      nReg(e1) must_== eReg(e1)
      nReg.size must_== eReg.size
    }
    "not introduce duplicate edges while merging two registries" in {
      val sum = eReg + (EdgeRegistry() + e1)
      sum.size must_== eReg.size
      sum(e1) must beSome(0)
    }
    "return its references" in {
      eReg.references  must contain(0,1,2).exactly
      eRegP.references must_== Set(0)
    }
    "return its registered edges" in {
      eReg.values  must contain(e1,e2,e3).exactly
      eRegP.values must_== Set(e4)
    }
    "support the definition of an adjacency relationship" in {
      def checkTriangle(p: Point, ps: (Point, Point)) = {
        eReg getAdjacencyFor vRegP(p).get must containTheSameElementsAs(Seq(vRegP(ps._1).get,vRegP(ps._2).get))
      }
      checkTriangle(p3, (p4,p5))
      checkTriangle(p4, (p3,p5))
      checkTriangle(p5, (p4,p3))
    }
  }

  "A Face Registry" should {
    "be empty when initialised" in { FaceRegistry().size must_== 0 }
    "support functional adding" in {
      val init = FaceRegistry()
      val reg = init + Face(0,Seq())
      init.size must_== 0
      reg.size must_== 1
    }
    "support adding as a sequential operator" in {
      fReg(0) must_== f1
      fReg(1) must_== f2
    }
    "support looking for a given face" in {
      fReg(f1) must beSome(0)
      fReg(f2) must beSome(1)
    }
    "support the + operator" in { (fReg + fRegP).size must_== 3 }
    "change index of its right parameter" in {
      val sum = fReg + fRegP
      sum(f1) must beSome(0)
      sum(f2) must beSome(1)
      sum(f3) must beSome(2)
    }
    "not introduce duplicates while adding a face into a registry" in {
      val nReg = fReg + f1
      nReg(f1) must_== fReg(f1)
      nReg.size must_== fReg.size
    }
    "not introduce duplicates while merging two registries" in {
      val sum = fReg + (FaceRegistry() + f1)
      sum.size must_== fReg.size
      sum(f1) must beSome(0)
    }
    "support a search-by-center mechanisms" in {
      fReg.lookFor(0)    must beSome(0)
      fReg.lookFor(10)   must beSome(1)
      fReg.lookFor(100)  must beNone
    }
    "return its references" in {
      fReg.references must contain(0,1).exactly
      fRegP.references must_== Set(0)
    }
    "return its registered faces" in {
      fReg.values must contain(f1,f2).exactly
      fRegP.values must_== Set(f3)
    }
  }


}