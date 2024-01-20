package eu.ace_design.island.io

import eu.ace_design.island.geom.MeshBuilderTestDataSet
import org.json.JSONObject
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MeshFactoryTest extends SpecificationWithJUnit {

  "MeshFactoryTest Specifications".title

  val mesh = MeshBuilderTestDataSet.mesh

  "The mesh serialization process" should {

    val json = MeshFactory(mesh)

    "produce a JSON object as output" in {
     json must beAnInstanceOf[JSONObject]
    }

    "serialize the size of the mesh" in {
      json.getInt("size") must_== mesh.size.get
    }

    "serialize the UUID of the mesh" in {
      json.getString("uuid") must_== mesh.uuid.toString
    }

    "serialize the vertices defined in the mesh, respecting the order" in {
      val vertices = json.getJSONArray("vertices")
      (0 until mesh.vertices.size) foreach { cpt =>
        mesh.vertices(cpt).x must_== vertices.getJSONArray(cpt).getDouble(0)
        mesh.vertices(cpt).y must_== vertices.getJSONArray(cpt).getDouble(1)
      }
      mesh.vertices.size must_== vertices.length()
    }

    "serialize the edges defined in the mesh, respecting the order" in {
      val edges = json.getJSONArray("edges")
      (0 until mesh.edges.size) foreach { cpt =>
        mesh.edges(cpt).p1 must_== edges.getJSONArray(cpt).getInt(0)
        mesh.edges(cpt).p2 must_== edges.getJSONArray(cpt).getInt(1)
      }
      mesh.edges.size must_== edges.length()
    }

    /*
    "serialize the faces defined in the mesh, respecting the order" in {
      val faces = json.getJSONArray("faces")
      (0 until mesh.faces.size) foreach { cpt =>
        // center
        val f = faces.getJSONObject(cpt)
        mesh.faces(cpt).center must_== f.getInt("center")
        // stored edges
        val storedEdges =  for (i <- 0 until f.getJSONArray("edges").length)
          yield f.getJSONArray("edges").getInt(i)
        mesh.faces(cpt).edges  must_== storedEdges
        // optional neighbors
        if (mesh.faces(cpt).neighbors.isDefined) {
          val storedNeighbors = for(i <- 0 until f.getJSONArray("neighbors").length)
            yield f.getJSONArray("neighbors").getInt(i)
          mesh.faces(cpt).neighbors.get must_== storedNeighbors
        } else {
          f.has("neighbors") must beFalse
        }

      }
      mesh.faces.size must_== faces.length()
    }

     */
  }

  "The mesh deserialization process" should {

    "read a JSON object and load it as a mesh" in {
      val json = MeshFactory(mesh)
      val meshPrime = MeshFactory(json)
      meshPrime must_== mesh
    }

    "read a string containing a JSON object  and load it as a mesh" in {
      val str = MeshFactory(mesh).toString
      val meshPrime = MeshFactory(str)
      meshPrime must_== mesh
    }

  }
}