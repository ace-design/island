package eu.ace_design.island.viewer

import eu.ace_design.island.geom._
import eu.ace_design.island.map.{IslandMap, HasForHeight, PropertySet}

/**
 * Helpers to factorize code shared by the JSON and OBJ viewers
 */
object ViewerHelpers {

  /**
   * For a given mesh, returns the involved vertices as a sequence (order matters) of double triples.
   * @param map the map containing the vertices
   * @return a tuple-based representation of each points
   */
  def buildVertices(map: IslandMap): Seq[(Double, Double, Double)] = {
    for(index <- 0 until map.vertexRefs.size) yield {
      val x: Double = map.vertex(index).x
      val y: Double = map.vertex(index).y
      val z: Double = try { map.vertexProps.getValue(index, HasForHeight()) } catch { case e: IllegalArgumentException => 0.0 }
      (x, y, z)
    }
  }

  /**
   * Transform the faces stored in the mesh into a sequence of face references
   * @param map the map containing the faces, edges and vertices
   * @return a sequence of face description, referencing vertices as a convex hull of each faces
   */
  def buildFaces(map: IslandMap): Seq[Seq[Int]] = {
    import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
    val data = map.faceRefs.toSeq.sorted map { idx =>
      val involved = map.cornerRefs(map.face(idx))
      // We need to build the convex hull of the polygon to obtain a convex representation of the face
      val coords = (involved map { i => map.vertex(i) } map { p => new Coordinate(p.x, p.y)}).toSeq
      val linear = coords :+ new Coordinate(coords(0).x, coords(0).y)
      val factory = new GeometryFactory()
      val convexCoords = factory.createPolygon(linear.toArray).convexHull.getCoordinates
      // Mapping back the convex polygon to vertices references
      val indexes = convexCoords map { c => map.vertexRef(Point(c.x, c.y))}
      indexes.slice(1, indexes.size).toSeq // removing the last one (the face is not a closed path)
    }
    (Seq[Seq[Int]]() /: data) { (acc, face) => acc :+ face }
  }

}
