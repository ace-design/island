package eu.ace_design.island.geom

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

/**
 * Public data set
 */
object MeshBuilderTestDataSet {

  val builder = new MeshBuilder(200)
  val grid4 = Set(Point(50,50), Point(50,150), Point(150,50), Point(150, 150))
  val mesh = builder(grid4)

  /*************************************************************************************************
   ** Helper functions to retrieve references based on known coordinates, in the computed dataset **
   *************************************************************************************************/

  // Looks for a point reference stored in mesh, based on pre-existing coordinates
  def p(x: Int,y: Int): Int = mesh.vertices(points(x,y)).get
  // Looks for an edge reference stored in mesh, based on pre-existing coordinates for p1 and p2
  def e(p1: (Int,Int), p2: (Int,Int)):Int = mesh.edges(Edge(p(p1._1,p1._2), p(p2._1, p2._2))).get
  // Looks for a face reference stored in mesh, based on pre-existing coordinates (its center)
  def f(x: Int,y: Int): Int = mesh.faces.lookFor(p(x,y)).get

  /******************************************************
   ** Helper function to explore the expected data set **
   ******************************************************/
  def getExpectedFace(f: Face): Face = {
    val center = mesh.vertices(f.center)
    faces((center.x, center.y))
  }


  /*************************************************************
   ** Points, Edges and Faces for a minimalist but known mesh **
   *************************************************************
   *                                                           *
   *       (0,0)     (0,100)   (0,200)                         *
   *         +---------+---------+                             *
   *         | (50,50) | (50,150)|                             *
   *         |    x    |    x    |                             *
   *         |         |         |                             *
   * (100,0) +---------+---------+ (100, 200)                  *
   *         |(150,50) |(150,150)|                             *
   *         |    x    |    x    |                             *
   *         |         |         |                             *
   *         +---------+---------+                             *
   *     (200,0)   (200,100)  (200,200)                        *
   *                                                           *
   *************************************************************/

  val points: Map[(Int, Int), Point] = Map(
    (0  ,0)  -> Point(0.0, 0.0),    (0  ,100) -> Point(0.0, 100.0),   (0  ,200) -> Point(0.0, 200.0),
    (50 ,50) -> Point(50.0, 50.0),  (50,150)  -> Point(50.0, 150.0),
    (100,0)  -> Point(100.0, 0.0),  (100,100) -> Point(100.0, 100.0), (100,200) -> Point(100.0, 200.0),
    (150,50) -> Point(150.0, 50.0), (150,150) -> Point(150.0, 150.0),
    (200,0)  -> Point(200.0, 0.0),  (200,100) -> Point(200.0, 100.0), (200,200) -> Point(200.0, 200.0)
  )


  val f00 = Face(p(50,50),
                 Seq(e((0,0), (0,100)), e((0,100),(100,100)), e((100,100),(100,0)), e((100,0),(0,0))),
                 Some(Set(f(50,150), f(150,50), f(150,150))))
  val f01 = Face(p(50,150),
                 Seq(e((0,100), (0,200)), e((0,200),(100,200)), e((100,200),(100,100)), e((100,100),(0,100))),
                 Some(Set(f(50,50), f(150,50), f(150,150))))
  val f10 = Face(p(150,50),
                 Seq(e((100,0), (100,100)), e((100,100),(200,100)), e((200,100),(200,0)), e((200,0),(100,0))),
                 Some(Set(f(50,50), f(50,150), f(150,150))))
  val f11 = Face(p(150,150),
                 Seq(e((100,100),(100,200)),e((100,200),(200,200)),e((200,200),(200,100)),e((200,100),(100,100))),
                 Some(Set(f(50,50), f(50,150), f(150,50))))

  val faces: Map[(Double,Double), Face] = Map((50.0,50.0)  -> f00, (50.0,150.0)  -> f01,
                                              (150.0,50.0) -> f10, (150.0,150.0) -> f11)

}

@RunWith(classOf[JUnitRunner])
class MeshBuilderTest extends SpecificationWithJUnit {
  import MeshBuilderTestDataSet._

  "MeshBuilderTest Specifications".title

  "A MeshBuilder" should {

    "produce a mesh when applied to a set of points" in {
      mesh must beAnInstanceOf[Mesh]
      mesh.vertices.size must_== 13
      mesh.edges.size must_== 12
      mesh.faces.size must_== 4
    }
  }

  "The built mesh" should {

    "contain the input points in its registry" in {
      mesh.vertices(points(50 , 50)) must beSome
      mesh.vertices(points(50 ,150)) must beSome
      mesh.vertices(points(150, 50)) must beSome
      mesh.vertices(points(150,150)) must beSome
    }

    "contain voronoi-based vertex used to separate regions in the map" in {
      mesh.vertices(points(0  ,  0))  must beSome
      mesh.vertices(points(100,  0))  must beSome
      mesh.vertices(points(200,  0))  must beSome
      mesh.vertices(points(0  , 100)) must beSome
      mesh.vertices(points(100, 100)) must beSome
      mesh.vertices(points(200, 100)) must beSome
      mesh.vertices(points(0  , 200)) must beSome
      mesh.vertices(points(100, 200)) must beSome
      mesh.vertices(points(200, 200)) must beSome
    }

    "contain all the edges used to build the different faces" in {
      def p(i: Int, j: Int) = MeshBuilderTestDataSet.p(i,j)
      mesh.edges(Edge(p(0,0),     p(0, 100)))   must beSome
      mesh.edges(Edge(p(0,100),   p(0, 200)))   must beSome
      mesh.edges(Edge(p(0,0),     p(100, 0)))   must beSome
      mesh.edges(Edge(p(0,100),   p(100, 100))) must beSome
      mesh.edges(Edge(p(0,200),   p(100, 200))) must beSome
      mesh.edges(Edge(p(100,0),   p(100, 100))) must beSome
      mesh.edges(Edge(p(100,100), p(100, 200))) must beSome
      mesh.edges(Edge(p(100,0),   p(200, 0)))   must beSome
      mesh.edges(Edge(p(100,100), p(200, 100))) must beSome
      mesh.edges(Edge(p(100,200), p(200, 200))) must beSome
      mesh.edges(Edge(p(200,0),   p(200, 100))) must beSome
      mesh.edges(Edge(p(200,100), p(200, 200))) must beSome
    }

    "contain all the faces that should be present in this mesh" in {
      def check(f: Face) = f.edges.toSet must_== getExpectedFace(f).edges.toSet
      check(mesh.faces(0))
      check(mesh.faces(1))
      check(mesh.faces(2))
      check(mesh.faces(3))
    }

    "identify the right neighborhood relationships" in  {
      def check(f: Face) = f.neighbors must_== getExpectedFace(f).neighbors
      check(mesh.faces(0))
      check(mesh.faces(1))
      check(mesh.faces(2))
      check(mesh.faces(3))
    }.pendingUntilFixed
  }

}