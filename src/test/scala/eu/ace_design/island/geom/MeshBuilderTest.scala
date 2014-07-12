package eu.ace_design.island.geom

import eu.ace_design.island.MeshBuilder
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MeshBuilderTest extends SpecificationWithJUnit {

  "MeshBuilderTest Specifications".title

  val builder = new MeshBuilder(200)

  /**
   *       (0,0)     (0,100)   (0,200)
   *         +---------+---------+
   *         | (50,50) | (50,150)|
   *         |    x    |    x    |
   *         |         |         |
   * (100,0) +---------+---------+ (100, 200)
   *         |(150,50) |(150,150)|
   *         |    x    |    x    |
   *         |         |         |
   *         +---------+---------+
   *     (200,0)   (200,100)  (200,200)
   */
  val points: Map[(Int, Int), Point] = Map(
    (0  ,0)  -> Point(0.0, 0.0),    (0  ,100) -> Point(0.0, 100.0),   (0  ,200) -> Point(0.0, 200.0),
    (50 ,50) -> Point(50.0, 50.0),  (50,150)  -> Point(50.0, 150.0),
    (100,0)  -> Point(100.0, 0.0),  (100,100) -> Point(100.0, 100.0), (100,200) -> Point(100.0, 200.0),
    (150,50) -> Point(150.0, 50.0), (150,150) -> Point(150.0, 150.0),
    (200,0)  -> Point(200.0, 0.0),  (200,100) -> Point(200.0, 100.0), (200,200) -> Point(200.0, 200.0)
  )
  val grid4 = Set(points(50,50), points(50,150), points(150,50), points(150, 150))
  val mesh = builder(grid4)

  def p(x: Int,y: Int): Int = mesh.vertices(points(x,y)).get
  def e(p1: (Int,Int), p2: (Int,Int)) = mesh.edges(Edge(p(p1._1,p1._2), p(p2._1, p2._2))).get

  val faces: Map[(Double,Double), Face] = Map(
    (50.0,50.0) -> Face(p(50,50),
                    Seq(e((0,0), (0,100)), e((0,100),(100,100)), e((100,100),(100,0)), e((100,0),(0,0)))),
    (50.0,150.0) -> Face(p(50,150),
                     Seq(e((0,100), (0,200)), e((0,200),(100,200)), e((100,200),(100,100)), e((100,100),(0,100)))),
    (150.0,50.0) -> Face(p(150,50),
                     Seq(e((100,0), (100,100)), e((100,100),(200,100)), e((200,100),(200,0)), e((200,0),(100,0)))),
    (150.0,150.0) -> Face(p(150,150),
                      Seq(e((100,100),(100,200)),e((100,200),(200,200)),e((200,200),(200,100)),e((200,100),(100,100)))))


  "A MeshBuilder" should {

    "produce a mesh when applied to a set of points" in {
      mesh must beAnInstanceOf[Mesh]
      mesh.vertices.size must_== 13
      mesh.edges.size must_== 12
      mesh.faces.size must_== 4
    }

    "contain the input points in its registry" in {
      mesh.vertices(points(50 , 50)) must beSome
      mesh.vertices(points(50 ,150)) must beSome
      mesh.vertices(points(150, 50)) must beSome
      mesh.vertices(points(150,150)) must beSome
    }

    "contain voronoi-based points used to separate regions in the map" in {
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
      def isValid(f: Face): Boolean = {
        val center = mesh.vertices(f.center)
        val expected = faces((center.x, center.y))
        f.edges.toSet == expected.edges.toSet
      }
      isValid(mesh.faces(0)) must beTrue
      isValid(mesh.faces(1)) must beTrue
      isValid(mesh.faces(2)) must beTrue
      isValid(mesh.faces(3)) must beTrue
    }
  }

}