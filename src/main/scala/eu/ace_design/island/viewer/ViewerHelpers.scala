package eu.ace_design.island.viewer

import eu.ace_design.island.geom._
import eu.ace_design.island.map.{HasForHeight, PropertySet}

/**
 * Helpers to factorize code shared by the JSON and OBJ viewers
 */
object ViewerHelpers {

  /**
   * For a given mesh, returns the involved vertices as a sequence (order matters) of double triples.
   * @param mesh the mesh to process
   * @return a tuple-based representation of each points
   */
  def buildVertices(mesh: Mesh, props: PropertySet): Seq[(Double, Double, Double)] = {
    // TODO support the z coordinates exploiting the properties!
    for(idx <- 0 until mesh.vertices.size)
      yield {
        val x: Double = mesh.vertices(idx).x
        val y: Double = mesh.vertices(idx).y
        val z: Double = try { props.getValue(idx, HasForHeight()) } catch { case e: IllegalArgumentException => 0.0 }
        (x, y, z)
      }
  }

  /**
   * Transform the faces stored in the mesh into a sequence of face references
   * @param mesh the mesh storing the faces, edges and vertices
   * @return a sequence of face description, referencing vertices as a convex hull of each faces
   */
  def buildFaces(mesh: Mesh): Seq[Seq[Int]] = {
    import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
    val data = (0 until mesh.faces.size) map { idx =>
      val f = mesh.faces(idx)
      val involved = f.vertices(mesh.edges)
      // We need to build the convex hull of the polygon to obtain a convex representation of the face
      val coords = (involved map { mesh.vertices(_) } map { p => new Coordinate(p.x, p.y)}).toSeq
      val linear = coords :+ new Coordinate(coords(0).x, coords(0).y)
      val factory = new GeometryFactory()
      val convexCoords = factory.createPolygon(linear.toArray).convexHull.getCoordinates
      // Mapping back the convex polygon to vertices references
      val indexes = convexCoords map { c => mesh.vertices(Point(c.x, c.y)).get}
      indexes.slice(1, indexes.size).toSeq // removing the last one (the face is not a closed path)
    }
    (Seq[Seq[Int]]() /: data) { (acc, face) => acc :+ face }
  }

}
