package eu.ace_design.island

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MeshBuilderTest extends SpecificationWithJUnit {

  "MeshBuilderTest Specifications".title

  "A MeshBuilder" should {
    val builder = new MeshBuilder(200)
    val points = Seq(Point(50, 50), Point(150,50), Point(50, 150), Point(150, 150))

    "produce a mesh when applied to a set of points" in {
      builder(points.toSet) must beAnInstanceOf[Mesh]
    }

    "contains the input points in its registry" in {
      val mesh = builder(points.toSet)
      mesh.registry(points(0)) must beSome
      mesh.registry(points(1)) must beSome
      mesh.registry(points(2)) must beSome
      mesh.registry(points(3)) must beSome
    }
  }


}