package eu.ace_design.island.geom.generators

import eu.ace_design.island.geom.Point
import scala.util.Random

/**
 * This generator generates a set of pseudo-random points, according to a standard uniform distribution law
 * @param size  the size of the PointGenerator
 * @param random the random generator to be used (by default, instantiating a new one)
 */
class RandomGrid(override val size: Int, override val random: Random = new Random()) extends RandomPointGenerator {

  /**
   * Actually computes the set of points to be generated. This method should be overridden in actual generators
   * @param n the number of point to generate
   * @return a set of points that matches the generation semantics of this generator
   */
  override protected def run(n: Int): Set[Point] = {
    val seq = for (i <- 1 to n) yield Point(random.nextDouble() * size, random.nextDouble() * size)
    seq.toSet
  }
}
