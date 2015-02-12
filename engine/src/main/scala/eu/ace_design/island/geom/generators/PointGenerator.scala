package eu.ace_design.island.geom.generators

import eu.ace_design.island.geom.Point

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
 * A random point generator is a point generator that use random numbers to distribute the position
 */
trait RandomPointGenerator extends PointGenerator {
  // The random generator to be used
  val random: scala.util.Random
}
