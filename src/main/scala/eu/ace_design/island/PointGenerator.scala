package eu.ace_design.island

/**
 * This file is part of the Island project.
 * @author mosser
 **/


/**
 * A PointGenerator generates a given number of Points according to the surface (height x width) to map
 */
trait PointGenerator {

  /**
   * The apply method allows one to call a PointGenerator as a function used to generate a point grid.
   * @param n the number of point to generate
   * @return a set of points that matches the generation semantics of this generator
   */
  def apply(n: Int): Set[Point] = {
    require(n > 0, "n must be positive")
    require(size > 0, "size must be positive")
    run(n)
  }

  /**
   * The size property of this PointGenerator. The map is assumed to be a size x size square.
   * @return the height of the map used to generate the points
   */
  def size: Int

  /**
   * Actually computes the set of points to be generated. This method should be overridden in actual generators
   * @param n the number of point to generate
   * @return a set of points that matches the generation semantics of this generator
   */
  protected def run(n: Int): Set[Point]
}


/**
 * This PointGenerator generates points aligned on a grid.
 * @param size the size of the PointGenerator
 */
class SquaredGrid(override val size: Int) extends PointGenerator {
  import scala.math._

  /**
   * This generator generates points aligned on a squared grid.
   *
   *   +---------+
   *   | x  x  x |
   *   | x  x  x |
   *   | x  x  x |
   *   +---------+
   *
   * @param n the number of points to generates (assumed a squared number)
   * @return a set of points aligned on a squared grid
   */
  override protected def run(n: Int): Set[Point] = {
    require(round(sqrt(n)) * round(sqrt(n)) == n, "n must be a squared number")

    val pointsByLine = sqrt(n).toInt
    val squareSide = size / sqrt(n)
    val halfSide = squareSide / 2

    val points = for(x <- 0 until pointsByLine; y <- 0 until pointsByLine)
      yield Point(x * squareSide + halfSide, y * squareSide + halfSide)

    points.toSet

  }

}

/**
 * This generator generates a set of pseudo-random points, according to a standard uniform distribution law
 * @param size  the size of the PointGenerator
 */
class RandomGrid(override val size: Int) extends PointGenerator {
  import scala.util.Random

  /**
   * Actually computes the set of points to be generated. This method should be overridden in actual generators
   * @param n the number of point to generate
   * @return a set of points that matches the generation semantics of this generator
   */
  override protected def run(n: Int): Set[Point] = {
    val random = new Random()
    val seq = for (i <- 1 to n) yield Point(random.nextDouble() * size, random.nextDouble() * size)
    seq.toSet
  }
}