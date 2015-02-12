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
    val data = map.faceRefs.toSeq.sorted map { idx =>
      val convexHull = map.convexHull(map.face(idx))
      // Mapping back the convex polygon to vertices references
      val indexes = convexHull map { c => map.vertexRef(Point(c.x, c.y))}
      indexes.slice(1, indexes.size).toSeq // removing the last one (the face is not a closed path)
    }
    (Seq[Seq[Int]]() /: data) { (acc, face) => acc :+ face }
  }

}
