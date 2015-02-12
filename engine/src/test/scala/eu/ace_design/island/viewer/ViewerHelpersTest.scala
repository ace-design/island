package eu.ace_design.island.viewer

import eu.ace_design.island.map.{IslandMap, HasForHeight, PropertySet}
import org.specs2.mutable._


class ViewerHelpersTest extends SpecificationWithJUnit {

  "ViewerHelpersTest Specifications".title

  val mesh = eu.ace_design.island.geom.MeshBuilderTestDataSet.mesh
  val vProps = (PropertySet() /: mesh.vertices.references) { (acc, ref) => acc + (ref -> HasForHeight(ref)) }
  val map = IslandMap(mesh).copy(vertexProps = vProps)

  "The ViewerHelper" should {

    "extract the vertices stored in a mesh, in the very same order" in {
      val data = ViewerHelpers.buildVertices(map)
      (0 until data.size) foreach {index =>
        val reference = map.vertex(index)
        (reference.x, reference.y) must_== (data(index)._1, data(index)._2)
        vProps.getValue(index, HasForHeight()) must_== data(index)._3
      }
      data must haveSize(mesh.vertices.size)
    }

    "extract the faces stored in a mesh, in the very same order" in {
      val data = ViewerHelpers.buildFaces(map)
      (0 until data.size) foreach { index =>
        val reference = map.face(index)
        reference.vertices(mesh.edges) must containTheSameElementsAs(data(index))
      }
      data must haveSize(mesh.faces.size)
    }
  }


}