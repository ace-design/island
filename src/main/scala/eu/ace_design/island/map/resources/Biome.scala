package eu.ace_design.island.map.resources

import eu.ace_design.island.stdlib.Resources._

import scala.util.Random

/**
 * This file is part of the island project
 * @author mosser (02/01/2015, 18:44)
 **/
trait Biome {

  val code: String
  protected val production: Seq[(Resource, Double)]
  val color: java.awt.Color

  /**
   * Return the resource to be associated to a given biome, using a given random generator
   * @param rand the random generator to be used
   * @return a Resource
   */
  def apply(rand: Random = new Random()): Resource = {
    val thresholds = propagate(production)
    val value = rand.nextDouble()
    val result = thresholds((thresholds.keys filter { _ > value }).min)
    result
  }

  /**
   * Transform a sequence of resources into a map binding probability threshold to resources
   * For example, consider the following sequence of resources: Seq((WOOD, 0.4), (SUGAR_CANE, 0.4), (FRUITS, 0.2)). The
   * function propagates the probability values to define intervals: Map(1.0 -> FRUITS, 0.8 -> SUGAR_CANE, 0.4 -> WOOD).
   * Considering a random number in [0,1], a value in [0, 0.4] will produce wood, a value in ]0.4, 0.8] will produce
   * sugar cane, and so on.
   * @param resources an element defined in _bindings
   * @return
   */
  private def propagate(resources: Seq[(Resource, Double)]): Map[Double, Resource] = {
    def loop(lst: Seq[(Resource, Double)], inc: Double):  Map[Double, Resource] = lst match {
      case Nil => Map[Double, Resource]()
      case (r, prob) :: others => loop(others, prob + inc) + ((prob + inc) -> r)
    }
    val res = loop(resources,0.0)
    if (res.isEmpty || res.keys.max < 1.0) res + (1.0 -> NoResource) else res
  }

}
