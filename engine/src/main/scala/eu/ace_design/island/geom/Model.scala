package eu.ace_design.island.geom

import java.util.UUID

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
case class Point(x: Double, y: Double) {
  require(x >= 0, "The x coordinate cannot be negative")
  require(y >= 0, "The y coordinate cannot be negative")

  /**
   * Compute the distance between two points (as a double)
   * @param that the other point
   * @return the distance between this and that, thanks to the Pythagorean theorem
   */
  def -->(that: Point): Double = math.sqrt(math.pow(this.x - that.x, 2) + math.pow(this.y - that.y, 2))
}

/**
 * An immutable edge is a line that goes from a given vertex to another one. Vertex are not stored directly, but instead
 * stored by their reference in a VertexRegistry (this is actually quite classical in geometrical libraries).
 *
 * Remark: p1 and p2 **must** be valid references in the very same VertexRegistry.
 *
 * @param p1 the first point reference
 * @param p2 the second point reference
 */
class Edge(val p1: Int, val p2: Int) {
  require(p1 != p2, "Vertices references must be different")

  /**
   * Checks if this edge involve a given point (as p1 or p2)
   * @param pRef the point to check
   * @return true it pRef is involved in this, false elsewhere.
   */
  def involves(pRef: Int): Boolean = this.p1 == pRef || this.p2 == pRef

  /**
   * Two edges are equals since they point to the same vertices, in any order (Edge(p,p') == Edge(p',p)).
   * @param other  an object to be tested for equality with this.
   * @return
   */
  override def equals(other: Any) = other match {
    case that: Edge => (this.p1 == that.p1 && this.p2 == that.p2) || (this.p1 == that.p2 && this.p2 == that.p1)
    case _ => false
  }

  /**
   * HashCode method to support storage in maps
   * @return the sum of the hashcode of each integer used as point reference
   */
  override def hashCode(): Int = Set(p1,p2).hashCode()

  /**
   * Mimic the case class default toString method usually generated automatically by the scala compiler
   * @return
   */
  override def toString: String = s"Edge({$p1,$p2})"
}

/**
 * A companion object to mimic a case class for the Edge concept.
 */
object Edge {
  /**
   * The apply method allow one to build an Edge without using the new operator (as Point and Face are case classes)
   * @param p1 first point reference
   * @param p2 2nd point reference
   * @return an instance of Point
   */
  def apply(p1: Int, p2: Int): Edge = { new Edge(p1,p2) }
}


/**
 * A Face is defined by a center, and a sequence of edges that draws its border.
 * @param center    The vertex reference for the center of this face
 * @param edges     The edges references that draw the border of this face
 * @param neighbors Optional references to the faces that share a border with this, default is None
 */
case class Face(center: Int, edges: Seq[Int], neighbors: Option[Set[Int]] = None) {
  /**
   * Compute the vertices references associated to this (in its frontier), based on a given EdgeRegistry
   * @param eReg the edge registry to be used
   * @return a Set of Integers that are vertex references involved in this face.
   */
  def vertices(eReg: EdgeRegistry) = (edges map { eReg(_) } map { e => Set(e.p1, e.p2) }).flatten.toSet
}

/**
 * A Mesh contains the vertices, edges and faces associated to a given map
 * @param vertices the vertex registry  (default the empty one)
 * @param uuid an unique identifier used to identify this mesh
 */
case class Mesh(
  vertices: VertexRegistry = VertexRegistry(),
  edges:    EdgeRegistry   = EdgeRegistry(),
  faces:    FaceRegistry   = FaceRegistry(),
  size: Option[Int] = None,
  uuid: UUID = UUID.randomUUID) {

  /**
   * Clip a given mesh by specifying its size (this is purely for information purpose, and supposed consistent with the
   * content of the mesh).
   * @param i the size (side length of a square) used for clipping.
   * @return a mesh with the given parameter as size.
   */
  def clip(i: Int): Mesh = this.copy(size = Some(i))

}





