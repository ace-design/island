package eu.ace_design.island.geom

import org.specs2.mutable._

class ModelTest extends SpecificationWithJUnit {

  "Geometry Model Specifications".title

  /*********************************************************************
   ** Data objects used for tests purpose: 4 triangles sides by sides **
   *********************************************************************/

  val pReg = VertexRegistry() + Point(1,2) + Point(1,4) + Point(1,6) +
    Point(2,2) + Point(2,3) + Point(2,4) +  Point(4,5) +
    Point(3,1) + Point(3,3) + Point(3,5)

  val eReg = EdgeRegistry() + Edge(0,1) + Edge(1,2) +
    Edge(0,7) + Edge(0,8) + Edge(1,8) + Edge(1,9) + Edge(2,9) +
    Edge(7,8) + Edge(8,9)

  val fRegNoNeighbors = FaceRegistry() + Face(3, Seq(2,3,7)) + Face(4, Seq(3,0,4)) +
    Face(5, Seq(4,8,5)) + Face(6, Seq(5,6,1))


  /********************
   ** Specifications **
   ********************/

  "A Point" should {
    val x = 2.0; val y = 5.0;  val p = Point(x,y)
    "hold an X coordinate" in { p.x must_== x }
    "hold an Y coordinate" in { p.y must_== y }
  }

  "An Edge" should {
    "contain 2 points references" in {
      val e = Edge(0,1)
      e.p1 must_== 0
      e.p2 must_== 1
    }
    "reject two equivalent references" in { Edge(1,1) must throwAn[IllegalArgumentException] }
    "be constructed in any order (non-directional)" in {
      Edge(1,2) must_== Edge(2,1)
    }
  }

  "A Face" should {
    val face = Face(0, Seq(1,2,3,4), Some(Set(1,2)))
    "contain a center" in { face.center must_== 0 }
    "contain a sequence of edges" in { face.edges must contain(1,2,3,4).inOrder }
    "contain a set of neighbors references" in {
      face.neighbors     must beSome
      face.neighbors.get must contain(1,2)
    }
    "not contains neighbors by default" in { Face(0, Seq(1,2,3,4)).neighbors must beNone }
    "return the vertices involved in its frontier" in {
      fRegNoNeighbors(0).vertices(eReg) must contain(0,7,8).exactly
      fRegNoNeighbors(1).vertices(eReg) must contain(0,1,8).exactly
      fRegNoNeighbors(2).vertices(eReg) must contain(1,8,9).exactly
      fRegNoNeighbors(3).vertices(eReg) must contain(1,2,9).exactly
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
    "compute neighbors for a given face" in {
      val fourTriangles = Mesh(pReg, eReg, fRegNoNeighbors)
      fourTriangles.neighbors(0) must contain(1,2).exactly
      fourTriangles.neighbors(1) must contain(0,2,3).exactly
      fourTriangles.neighbors(2) must contain(0,1,3).exactly
      fourTriangles.neighbors(3) must contain(1,2).exactly
    }
  }


}