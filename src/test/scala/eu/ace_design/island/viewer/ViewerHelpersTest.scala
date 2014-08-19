package eu.ace_design.island.viewer

import eu.ace_design.island.map.{HasForHeight, PropertySet}
import org.specs2.mutable._


class ViewerHelpersTest extends SpecificationWithJUnit {

  "ViewerHelpersTest Specifications".title

  val mesh = eu.ace_design.island.geom.MeshBuilderTestDataSet.mesh
  val vProps = (PropertySet() /: mesh.vertices.references) { (acc, ref) => acc + (ref -> HasForHeight(ref)) }

  "The ViewerHelper" should {

    "extract the vertices stored in a mesh, in the very same order" in {
      val data = ViewerHelpers.buildVertices(mesh, vProps)
      (0 until data.size) foreach {index =>
        val reference = mesh.vertices(index)
        reference.x must_== data(index)._1
        reference.y must_== data(index)._2
        vProps.getValue(index, HasForHeight()) must_== data(index)._3
      }
      data must haveSize(mesh.vertices.size)
    }

    "extract the faces stored in a mesh, in the very same order" in {
      val data = ViewerHelpers.buildFaces(mesh)
      (0 until data.size) foreach { index =>
        val reference = mesh.faces(index)
        reference.vertices(mesh.edges) must containTheSameElementsAs(data(index))
      }
      data must haveSize(mesh.faces.size)
    }
  }


}