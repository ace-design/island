package eu.ace_design.island.geom

/**
 * This file is part of the island project. It is designed to be INDEPENDENT of any geometrical library.
 * It contains the different data structures and interfaces used to implement the Map geometry. Data structures are
 * defined in a functional and immutable way.
 * @author mosser
 **/


/**
 * A Point represents a location in a 2 dimensional plane
 * @param x coordinate according to the X axis
 * @param y coordinate according to the Y axis
 */
case class Point(x: Double, y: Double)


/**
 * A Mesh contains the vertices, edges and faces associated to a given map
 * @param vertices the vertex registry  (default the empty one)
 */
case class Mesh(
  vertices: VertexRegistry = VertexRegistry(),
  edges: EdgeRegistry      = EdgeRegistry(),
  faces: FaceRegistry      = FaceRegistry()) {

  /**
   * Return a new mesh that contains
   * @param p
   * @return
   */
  def +(p: Point) = this.copy(vertices = this.vertices + p)
}




case class Face()

case class Edge()

