package eu.ace_design.island


/**
 * This file is part of the island project. It is designed to be INDEPENDENT of any geometrical library.
 * It contains the different data structures and interfaces used to implement the Map geometry
 * @author mosser
 **/


/**
 * A point represents a location in a 2 dimensional plane
 * @param x coordinate according to the X axis
 * @param y coordinate according to the Y axis
 */
case class Point(x: Double, y: Double)


class Mesh {

}

/**
 * A VerticeRegistry store all the points used by a given mesh
 * @param _contents An ordered Sequence of Points, default is the empty sequence
 */
class VerticeRegistry(private val _contents: Seq[Point] = Seq()) {

  /**
   * Add a point to the registry in a functional way, at the end.
   * @param p the point to add
   * @return a new registry that containts the new point
   */
  def +(p: Point): VerticeRegistry = new VerticeRegistry(_contents :+ p)

  /**
   * The size of this registry (immutable)
   * @return the size of this
   */
  val size: Int = _contents.size

  /**
   * Access to the i_th element of the registry
   * @param i the index to retrieve
   * @return None if the index is out of point, Some(p) elsewhere (where p is located at index i)
   */
  def apply(i: Int): Option[Point] = if (i >= size ) None else Some(_contents(i))

  /**
   * Access to the index of a given point (exact match)
   * @param p the point to look for
   * @return None if the point is not in the registry, Some(i) elsewhere (where i is the index of p)
   */
  def apply(p: Point): Option[Int] = _contents.indexOf(p) match {
    case x if x < 0 => None
    case x => Some(x)
  }

}