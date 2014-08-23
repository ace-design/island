package eu.ace_design.island.geom.generators

import eu.ace_design.island.geom.Point

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
