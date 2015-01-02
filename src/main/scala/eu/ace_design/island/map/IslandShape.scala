package eu.ace_design.island.map

import eu.ace_design.island.geom.Point
import eu.ace_design.island.util.{LogSilos, Logger}
import scala.util.Random

/**
 * An IslandShape is a function used to decide if a vertex (not a face) is located on land or not
 */
trait IslandShape extends Logger {

  val silo = LogSilos.MAP_GEN

  import eu.ace_design.island.geom.Point
  require(size > 0, "The size of an Island cannot be negative or null")

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
  protected  def project(d: Double): Double = ((d / size.toDouble) - 0.5) * 2

  /**
   * Un-normalize a coordinate (in [-1,1]) according to the size of the map (=> projected into [0, size]).
   * @param n the normalized coordinate to un-project , in [-1,1]
   * @return the associated value in [0, size]
   */
  protected def unproject(n: Double): Double = (n / 2 + 0.5) * size.toDouble

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
  protected def normalize(r: Double): Double = (r/size.toDouble) * 2

  /**
   * Check if a point is located inside (or outside) a discus of given radius
   * @param x the x (normalized) coordinate of the point
   * @param y the y (normalized) coordinate of the point
   * @param radius the radius of the circle to check
   * @param checker a function (e.g., INSIDE or OUTSIDE) to decide for the point
   * @return the output of the checker when applied to sqrt(x2+y2) and the normalized radius
   */
  protected def distanceToCenter(x: Double, y: Double, radius: Double, checker: (Double, Double) => Boolean) = {
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
 * Implements a shape relying on a single disk (surface inside a circle, circle not included)
 * @param size an integer representing the size of the map
 * @param radius the radius of the circle
 */
case class DiskShape(override val size: Int, radius: Double) extends IslandShape with CircularHelper {
  require(radius > 0, "The disk radius cannot be negative")

  /**
   * (x,y) is located in water if it is located outside of the circle used to define the shape of the island
   * @param x the x coordinate, normalized (\in [-1,1])
   * @param y the y coordinate, normalized (\in [-1,1])
   * @return true if this point is in a water area, false elsewhere.
   */
  override protected def check(x: Double, y: Double): Boolean =  distanceToCenter(x, y, radius, OUTSIDE)
}

/**
 * Shape based on the Ellipsis cartesian formula. Will generate a circle if a = b
 * @param size the size of the map, unused here
 * @param a the % (in [0.0,1.0] of the x axis to be covered by the ellipsis
 * @param b the % (in [0.0,1.0] of the y axis to be covered by the ellipsis
 */
case class EllipsisShape(override val size: Int, a: Double, b: Double) extends IslandShape {
  require(a >= 0.0 && a <= 1.0, "a must be in [0.0, 1.0]")
  require(b >= 0.0 && b <= 1.0, "b must be in [0.0, 1.0]")

  // return true if the point (x,y) is outside the ellipsis
  override protected def check(x: Double, y: Double): Boolean = math.pow(x/a, 2) + math.pow(y/b, 2) >= 1.0
}

/**
 * Shape based on the Land of Oz map (a squared territory, bordered by an impassable desert)
 * @param size
 */
case class OzShape(override val size: Int) extends IslandShape {

  override protected def check(x: Double, y: Double): Boolean = false // Everything is considered as land
}

/**
 * Implements a shape made by two circles (internal and external). Produces island with a lake in the middle.
 * @param size the size of the map
 * @param radExt radius of the external circle (outer ocean)
 * @param radInt radius of the internal circle (inner lake)
 */
case class DonutShape(override val size: Int, radExt: Double, radInt: Double) extends IslandShape with CircularHelper {
  require(radExt > 0, "The external radius cannot be negative or null")
  require(radInt > 0, "The internal radius cannot be negative or null")
  require(radExt > radInt, "The external circle must be bigger than the internal one")

  /**
   * (x,y) is located in water if it is located outside of the circle used to define the shape of the island
   * @param x the x coordinate, normalized (\in [-1,1])
   * @param y the y coordinate, normalized (\in [-1,1])
   * @return true if this point is in a water area, false elsewhere.
   */
  override protected def check(x: Double, y: Double): Boolean = {
    distanceToCenter(x, y, radExt, OUTSIDE) || distanceToCenter(x, y, radInt, INSIDE)
  }
}


 /*******************
  ** Exotic Shapes **
  *******************/

/**
 * Re-implementation (and slight adaptation) of the algorithm designed by Amit Patel in
 * https://github.com/amitp/mapgen2/blob/master/Map.as (see makeRadial)   [MIT License]
 */
case class RadialShape(override val size: Int,
                       factor: Double,
                       random: Random = new Random()) extends IslandShape with Logger {

  import scala.math.{abs, atan2, cos, max, pow, sin, sqrt, Pi}

  private val bumps: Int = random.nextInt(5) + 1          // random number between 1 and 6
  val startAngle: Double = random.nextDouble() * 2 * Pi   // random double between 0 and 2π
  val dipAngle: Double   = random.nextDouble() * 2 * Pi     // random double between 0 and 2π
  val dipWidth: Double = 0.2 + (random.nextDouble() * 0.5)

  override protected def check(x: Double, y: Double): Boolean = {
    val angle = atan2(x,y)
    val length = 0.5 * (max(abs(x), abs(y)) + sqrt(pow(x,2) + pow(y,2)))

    val threshold: Boolean = abs(angle - dipAngle) < dipWidth ||
      abs(angle - dipAngle + 2 * Pi) < dipWidth ||
      abs(angle - dipAngle - 2 * Pi) < dipWidth

    val r1 = if (threshold) 0.2 else 0.5 + 0.4 * sin(startAngle + bumps * angle + cos((bumps + 3) * angle))
    val r2 = if (threshold) 0.2 else 0.7 - 0.2 * sin(startAngle + bumps * angle - sin((bumps + 2) * angle))

    !(length < r1 || (length > r1 * factor && length < r2))
  }
}
