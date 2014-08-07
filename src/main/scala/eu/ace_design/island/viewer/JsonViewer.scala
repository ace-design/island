package eu.ace_design.island.viewer

import java.io.{PrintWriter, File}
import eu.ace_design.island.geom._
import org.json
import org.json._
import eu.ace_design.island.map._
import eu.ace_design.island.util.Logger

/**
 * Transform a map into a JSON file. The JSON file is directly inspired by the OBJ WaveFront file format for
 * vertices and faces indexes.
 **/
class JsonViewer extends Viewer with Logger {

  override def extension: String = "json"
  override def mimeType: String = "application/json"

  override def apply(m: IslandMap): File = {

    info("Building JSON file")
    val vertices = buildVertices(m.mesh.vertices)
    val faces = buildFaces(m.mesh)
    val vProps = buildProperties(m.vertexProps)
    val fProps = buildProperties(m.faceProps)


    val geom = new JSONObject()
    geom.put("vertices", vertices)
    geom.put("faces", faces)


    val props = new JSONObject()
    props.put("vertices", vProps)
    props.put("faces", fProps)

    val result = new JSONObject()
    result.put("geometry", geom)
    result.put("properties", props)
    result.put("_comments", "JSON representation of an Island, automatically generated")

    val out = initOutput
    val writer = new PrintWriter(out, "UTF8")
    result.write(writer)
    writer.close()
    out
  }

  def buildProperties(reg: PropertySet): JSONArray = {
    val objects = reg.references map { ref =>
      val props = (new JSONArray() /: reg.get(ref)) { (acc, prop) => acc.put(prop.toString) }
      new JSONObject().put("i",ref).put("p",props)
    }
    (new JSONArray() /: objects) { (acc, obj) => acc.put(obj) }
  }


  /**
   * Transform the faces stored in the mesh into their OBJ representation
   * An OBJ face is a sequence of 3+ vertices references (starting at 1): "f v1 ... vN"
   * Specifying normals is optional (thus not done here)
   * @param mesh the mesh storing the faces, edges and vertices
   * @return a sequence of face description
   */
  private def buildFaces(mesh: Mesh): JSONArray = {
    import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
    info("Building faces index")
    val data = (0 until mesh.faces.size) map { idx =>
      val f = mesh.faces(idx)
      val involved = f.vertices(mesh.edges)
      // We need to build the convex hull of the polygon to obtain a convex representation of the face
      val coords = (involved map {
        mesh.vertices(_)
      } map { p => new Coordinate(p.x, p.y)}).toSeq
      val linear = coords :+ new Coordinate(coords(0).x, coords(0).y)
      val factory = new GeometryFactory()
      val convexCoords = factory.createPolygon(linear.toArray).convexHull.getCoordinates
      // Mapping back the convex polygon to vertices references
      val indexes = convexCoords map { c => mesh.vertices(Point(c.x, c.y)).get}
      val processed = indexes.slice(1, indexes.size) // removing the last one (the face is not a closed path)
      s"f ${processed.mkString(" ")}"
      (new JSONArray() /: processed) { (acc, ref) => acc.put(ref)}
    }
    (new JSONArray() /: data) { (acc, face) => acc.put(face)}
  }


  /**
   * Build the vertices registry (sequence of vertex description, as 3 slots arrays)
   * A vertex is represented as "[$x $y $z]"
   * @param vReg the vertex registry used to store the ≠ vertices
   * @return a sequence of vertex description
   */
  private def buildVertices(vReg: VertexRegistry): JSONArray = {
    info("Building vertices index")
    // TODO support the z coordinates exploiting the properties!
    val data = for(idx <- 0 until vReg.size)
      yield new JSONArray().put(vReg(idx).x).put(vReg(idx).y).put(0.0)
    (new JSONArray() /: data) { (acc, vertex) => acc.put(vertex) }
  }





}
