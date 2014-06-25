package eu.ace_design.island

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MeshBuilderTest extends SpecificationWithJUnit {

  "MeshBuilderTest Specifications".title

  "A MeshBuilder" should {
    val points = Set()
    "produce a mesh when applied to a set of points" in {
      val builder = new MeshBuilder()

    }
  }


}