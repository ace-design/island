package eu.ace_design.island


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
 * @param registry the vertex registry  (default the empty one)
 */
case class Mesh(
  registry: VertexRegistry = new VertexRegistry()
  ) {

  /**
   * Return a new mesh that contains
   * @param p
   * @return
   */
  def +(p: Point) = this.copy(registry = this.registry + p)
}

/**
 * A VertexRegistry store all the points used by a given mesh
 * @param vertices A map binding points to indexes, default is the empty map
 */
case class VertexRegistry(
  vertices: Map[Point, Int] = Map()) {

  private lazy val _lookup: Map[Int, Point] = vertices map { _.swap }

  /**
   * Add a point to the registry in a functional way, at the end.
   * @param p the point to add
   * @return a new registry that contains the new point at the end
   */
  def +(p: Point): VertexRegistry =
    this.copy(vertices = this.vertices + (p -> this.size))

  /**
   * The size of this registry (immutable)
   * @return the size of this
   */
  val size: Int = vertices.size

  /**
   * Access to the i_th element of the registry
   * @param i the index to retrieve
   * @return None if the index is out of point, Some(p) elsewhere (where p is located at index i)
   */
  def apply(i: Int): Point = _lookup(i)

  /**
   * Access to the index of a given point (exact match)
   * @param p the point to look for
   * @return None if the point is not in the registry, Some(i) elsewhere (where i is the index of p)
   */
  def apply(p: Point): Option[Int] = vertices.get(p)

}