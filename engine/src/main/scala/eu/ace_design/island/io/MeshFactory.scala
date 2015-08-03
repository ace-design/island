package eu.ace_design.island.io


import java.util.UUID

import eu.ace_design.island.geom._
import org.json.{JSONArray, JSONObject}

/**
 * This object is used to transform a Mesh into a JSON object, or to load a mesh from a given JSON object
 **/
object MeshFactory {

  /**
   * Write a given mesh into a JSON object
   * @param mesh
   * @return
   */
  def apply(mesh: Mesh): JSONObject = MeshWriter(mesh)


  /**
   * Read a Mesh from a JSON object
   * @param obj the object to read
   * @return the associated mesh
   */
  def apply(obj: JSONObject): Mesh = MeshReader(obj)

  /**
   * Load a Mesh from a given JSON string
   * @param str the string to load
   * @return an instance of Mesh
   */
  def apply(str: String): Mesh = this(new JSONObject(str))


  /**
   * Writer object
   */
  private object MeshWriter {

    /**
     * Transform a mesh into the associated json representation
     * @param mesh
     * @return
     */
    def apply(mesh: Mesh): JSONObject = {
      require(mesh.size.isDefined, "Cannot serialize a mesh with unknown size")
      val json = new JSONObject()
      json.put("size", mesh.size.get)
      json.put("uuid", mesh.uuid.toString)
      writeVertices(mesh, json)
      writeEdges(mesh,json)
      writeFaces(mesh,json)
      json
    }

    /**
     * Create a JSON array containing the vertices used in this mesh (order respected)
     * Each vertex (x,y) is modelled as an array [x,y], stored as a sequence of vertices in an array named "vertices"
     * @param mesh
     * @param obj
     */
    private def writeVertices(mesh: Mesh, obj: JSONObject) {
      val data = new JSONArray()
      for(i <- 0 until mesh.vertices.size) {
        val point = mesh.vertices(i)
        data.put(new JSONArray().put(point.x).put(point.y))
      }
      obj.put("vertices",data)
    }

    /**
     * Create a JSON array containing the edges used in this mesh (preserving order)
     * Each edge p1 -> p2 is modelled as an array [p1, p2], stored as a sequence of edges in an array named "edges"
     * @param mesh
     * @param obj
     */
    private def writeEdges(mesh: Mesh, obj: JSONObject) {
      val data = new JSONArray()
      for(i <- 0 until mesh.edges.size) {
        val edge = mesh.edges(i)
        data.put(new JSONArray().put(edge.p1).put(edge.p2))
      }
      obj.put("edges",data)
    }

    /**
     * Create a JSON array containing the faces used in the mesh.
     * each face is transformed into a JSON object, stored as a sequence of faces in an array named "faces"
     * A face is a JSON object containing a center, edges and optionally neighbors
     * @param mesh
     * @param obj
     */
    private def writeFaces(mesh: Mesh, obj: JSONObject): Unit = {
      val data = new JSONArray()
      for(i <- 0 until mesh.faces.size) {
        val f = mesh.faces(i)
        val json = new JSONObject()
        // center
        json.put("center", f.center)
        // edges
        json.put("edges", (new JSONArray() /: f.edges) { _.put(_) }) // Shazam!
        // Optional neighbors
        f.neighbors match {
          case Some(neighbors) => json.put( "neighbors",
            (new JSONArray() /: f.neighbors.get) { _.put(_) } )
          case None =>
        }
        data.put(json)
      }
      obj.put("faces", data)
    }

  }

  /**
   * Reader object
   */
  private object MeshReader {

    def apply(obj: JSONObject): Mesh = {
      val vertices: VertexRegistry = readVertices(obj)
      val edges: EdgeRegistry      = readEdges(obj)
      val faces: FaceRegistry      = readFaces(obj)
      val size: Int = obj.getInt("size")
      val uuid: UUID = UUID.fromString(obj.getString("uuid"))
      Mesh(vertices, edges, faces, Some(size), uuid)
    }

    /**
     * Read vertices from a JSON array
     * @param obj
     * @return
     */
    def readVertices(obj: JSONObject): VertexRegistry = {
      val vertices = (0 until obj.getJSONArray("vertices").length) map { i =>
        val data = obj.getJSONArray("vertices").getJSONArray(i)
        Point(x = data.getDouble(0), y = data.getDouble(1))
      }
      (VertexRegistry() /: vertices) { _ + _ }
    }

    /**
      * Read edges from a JSON array
      * @param obj
      * @return
      */
    def readEdges(obj: JSONObject): EdgeRegistry = {
      val edges = (0 until obj.getJSONArray("edges").length) map { i =>
        val data = obj.getJSONArray("edges").getJSONArray(i)
        Edge(p1 = data.getInt(0), p2 = data.getInt(1))
      }
      (EdgeRegistry() /: edges) { _ + _ }
    }

    /**
     * Read faces from
     * @param obj
     * @return
     */
    def readFaces(obj: JSONObject): FaceRegistry = {
      val faces = (0 until obj.getJSONArray("faces").length) map { i =>
        val data = obj.getJSONArray("faces").getJSONObject(i)
        val center = data.getInt("center")
        val edges = (0 until data.getJSONArray("edges").length) map { data.getJSONArray("edges").getInt }
        val neighbors = data.has("neighbors") match {
          case false => None
          case true => {
            val ns = (0 until data.getJSONArray("neighbors").length) map { data.getJSONArray("neighbors").getInt }
            Some(ns.toSet)
          }
        }
        Face(center = center, edges = edges, neighbors = neighbors)
      }
      (FaceRegistry() /: faces) { _ + _ }
    }
  }

}







