package eu.ace_design.island.map.resources

import scala.util.Random

/**
 * This file is part of the island project
 * @author mosser (02/01/2015, 18:44)
 **/
trait Biome extends eu.ace_design.island.util.NameAsClassName {

  override def toString = name

  // the key code to be used in Whittaker diagrams to represent this biome
  val code: String

  /** the primary resource produced by this biome.
   * e.g., Seq((WOOD, 0.4), (SUGAR_CANE, 0.4), (FRUITS, 0.2)) means that this biome as 40% of chance to produce Wood,
   * 40% of chance to produce SUGAR_CANE and 20% of chance to produce FRUITS. The percentage might not sum to 1, meaning
   * that this biome can be sterile.
   **/
  protected val production: Seq[(PrimaryResource, Double)]

  // the color to be used to represent this Biome on the map
  val color: java.awt.Color

  /**
   * Return the resource to be associated to a given biome, using a given random generator
   * @param rand the random generator to be used
   * @return a Resource
   */
  def apply(rand: Random = new Random()): PrimaryResource = {
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
  private def propagate(resources: Seq[(PrimaryResource, Double)]): Map[Double, PrimaryResource] = {
    def loop(lst: Seq[(PrimaryResource, Double)], inc: Double):  Map[Double, PrimaryResource] = lst match {
      case Nil => Map[Double, PrimaryResource]()
      case (r, prob) :: others => loop(others, prob + inc) + ((prob + inc) -> r)
    }
    val res = loop(resources,0.0)
    if (res.isEmpty || res.keys.max < 1.0) res + (1.0 -> NoResource) else res
  }

}
