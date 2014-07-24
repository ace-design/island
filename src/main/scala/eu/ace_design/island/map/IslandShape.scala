package eu.ace_design.island.map

import eu.ace_design.island.geom.Point

/**
 * An IslandShape is a function used to decide if a vertex (not a face) is located on land or not
 */
trait IslandShape {

  // the size of the map (used for the normalization of Point's coordinates)
  val size: Int

  /**
   * Decide if a point is located on water or not (thus, on land)
   * @param p the point to check
   * @return true if this point is in a water area, false elsewhere.
   */
  def isWater(p: Point): Boolean = check(project(p.x), project(p.y))

  /**
   * Normalize a coordinate according to the size of the map. It project the coordinate into the space [-1, 1]
   * @param d the coordinate defined in [0, size]
   * @return the associated projection, in [-1, 1]
   */
  private def project(d: Double): Double = ((d/size) - 0.5) * 2

  /**
   * Check if a given couple of normalized coordinates is located in a water area, or not.
   * @param x the x coordinate, normalized (\in [-1,1])
   * @param y the y coordinate, normalized (\in [-1,1])
   * @return true if this point is in a water area, false elsewhere.
   */
  protected def check(x: Double, y: Double): Boolean

}


case class CircularShape(override val size: Int, radius: Double) extends IslandShape {
  import scala.math.{pow, sqrt}

  // Normalized radius
  private val nRad = (radius/size) * 2

  /**
   * (x,y) is located in water if it is located outside of the circle used to define the shape of the island
   * @param x the x coordinate, normalized (\in [-1,1])
   * @param y the y coordinate, normalized (\in [-1,1])
   * @return true if this point is in a water area, false elsewhere.
   */
  override protected def check(x: Double, y: Double): Boolean =  sqrt(pow(x,2) + pow(y,2)) > nRad
}

/**
 * This function implements a RadialShape, inspired by what was initially defined by Amit Patel in
 * https://github.com/amitp/mapgen2/blob/master/Map.as (see makeRadial)
 */
case class RadialShape(override val size: Int) extends IslandShape {
  // TODO Implement it  !!
  override protected def check(x: Double, y: Double): Boolean = ???
}
