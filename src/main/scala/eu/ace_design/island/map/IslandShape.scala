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


/*********************
 ** Circular Shapes **
 *********************/

/**
 * Define common methods used to handle circular island (e.g., is a given point inside a circle)
 */
trait CircularHelper extends IslandShape {
  import scala.math.{pow, sqrt}

  /**
   * Normalize a radius according to the size of the map (as the coordinates are in [-1,1]
   * @param r the radius to normalize
   * @return a non-negative integer representing this radius (in [0,1])
   */
  protected def normalize(r: Double): Double = (r/size) * 2

  /**
   * Check if a point is located inside (or outside) a circle of given radius
   * @param x the x (normalized) coordinate of the point
   * @param y the y (normalized) coordinate of the point
   * @param radius the radius of the circle to check
   * @param checker a function (e.g., INSIDE or OUTSIDE) to decide for the point
   * @return the output of the checker when applied to sqrt(x2+y2) and the normalized radius
   */
  protected def circleComparison(x: Double, y: Double, radius: Double, checker: (Double, Double) => Boolean) = {
    checker(sqrt(pow(x,2) + pow(y,2)), normalize(radius))
  }

  /**
   * Syntactic sugar to decide if a point is "inside" a circle.
   */
  protected final val INSIDE: (Double, Double) => Boolean = (x,y) => x < y

  /**
   * Syntactic sugar to decide if a point is "outside" a circle
   */
  protected final val OUTSIDE: (Double, Double) => Boolean = (x,y) => x > y

}

/**
 * Implements a shape relying on a single circle
 * @param size an integer representing the size of the map
 * @param radius the radius of the circle
 */
case class CircularShape(override val size: Int, radius: Double) extends IslandShape with CircularHelper {

  /**
   * (x,y) is located in water if it is located outside of the circle used to define the shape of the island
   * @param x the x coordinate, normalized (\in [-1,1])
   * @param y the y coordinate, normalized (\in [-1,1])
   * @return true if this point is in a water area, false elsewhere.
   */
  override protected def check(x: Double, y: Double): Boolean =  circleComparison(x, y, radius, OUTSIDE)
}

/**
 * Implements a shape made by two circles (internal and external). Produces island with a lake in the middle.
 * @param size the size of the map
 * @param radExt radius of the external circle (outer ocean)
 * @param radInt radius of the internal circle (inner lake)
 */
case class DonutShape(override val size: Int, radExt: Double, radInt: Double) extends IslandShape with CircularHelper {

  /**
   * (x,y) is located in water if it is located outside of the circle used to define the shape of the island
   * @param x the x coordinate, normalized (\in [-1,1])
   * @param y the y coordinate, normalized (\in [-1,1])
   * @return true if this point is in a water area, false elsewhere.
   */
  override protected def check(x: Double, y: Double): Boolean = {
    circleComparison(x, y, radExt, OUTSIDE) || circleComparison(x, y, radInt, INSIDE)
  }
}


 /*******************
  ** Exotic Shapes **
  *******************/

/**
 * This function implements a RadialShape, inspired by what was initially defined by Amit Patel in
 * https://github.com/amitp/mapgen2/blob/master/Map.as (see makeRadial)
 */
case class RadialShape(override val size: Int) extends IslandShape {
  // TODO Implement it  !!
  override protected def check(x: Double, y: Double): Boolean = ???
}
