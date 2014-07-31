package eu.ace_design.island.map

import eu.ace_design.island.geom.MeshBuilderTestDataSet
import org.specs2.mutable._

class IslandMapTest extends SpecificationWithJUnit {

  "MapTest Specifications".title

  "A map" should {
    val m = IslandMap(MeshBuilderTestDataSet.mesh)
    "rely on a mesh" in { m must beAnInstanceOf[IslandMap] }
    "do not contain any property when created" in {
      m.faceProps.size must_== 0
      m.vertexProps.size must_== 0
    }
  }

}