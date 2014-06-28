package eu.ace_design.island

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MeshBuilderTest extends SpecificationWithJUnit {

  "MeshBuilderTest Specifications".title

  "A MeshBuilder" should {
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

    "produce a mesh when applied to a set of points" in { builder(grid4) must beAnInstanceOf[Mesh] }

    "contains the input points in its registry" in {
      val mesh = builder(grid4)       ;
      mesh.registry(points(50 , 50)) must beSome
      mesh.registry(points(50 ,150)) must beSome
      mesh.registry(points(150, 50)) must beSome
      mesh.registry(points(150,150)) must beSome
    }

    "contains voronoi-based points used to separate regions in the map" in {
      val mesh = builder(grid4)
      mesh.registry(points(0  ,  0))  must beSome
      mesh.registry(points(100,  0))  must beSome
      mesh.registry(points(200,  0))  must beSome
      mesh.registry(points(0  , 100)) must beSome
      mesh.registry(points(100, 100)) must beSome
      mesh.registry(points(200, 100)) must beSome
      mesh.registry(points(0  , 200)) must beSome
      mesh.registry(points(100, 200)) must beSome
      mesh.registry(points(200, 200)) must beSome
    }
  }


}