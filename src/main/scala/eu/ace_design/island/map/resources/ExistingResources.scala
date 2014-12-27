package eu.ace_design.island.map.resources

import scala.util.Random

/**
 * Represents the resources available in the game
 **/
object ExistingResources extends Enumeration {
  type Resource = Value

  val None = Value // The none resource

  // Primary Resources
  val FISH, SILICA, ORE, WOOD, FRUITS, SUGAR_CANE, FLOWER, FUR = Value

  // Transformed Resources
  val GLASS, INGOT, PLANK, RUM, ELIXIR, LEATHER = Value
}


object BiomeToResource {
  import ExistingBiomes._
  import ExistingResources._

  /**
   * This binding maps a biome to the associated resources, with a given probability.
   * The semantics is the following: TEMPERATE_RAIN_FOREST -> Seq((WOOD,0.8),(FUR, 0.2)) means that a temperate rain
   * forest has 80% of chance of producing wood, and 20% of chance of producing furs.
   * The different probabilities might not sum to 1 (e.g., glacier), meaning that nothing (None)
   * can be produced by this biome
   */
  private val _bindings: Map[Biome, Seq[(Resource, Double)]] = Map(
    // Water faces
    LAKE -> Seq((FISH,0.8)), GLACIER -> Seq((FLOWER, 0.05)), OCEAN -> Seq((FISH,0.9)),
    // Land faces
    BEACH     -> Seq((SILICA, 0.5), (FISH, 0.5)),
    GRASSLAND -> Seq((FUR, 1.0)), TUNDRA -> Seq((FUR, 1.0)), SHRUBLAND -> Seq((FUR, 1.0)),
    TEMPERATE_DECIDUOUS_FOREST -> Seq((WOOD, 1.0)),
    TROPICAL_RAIN_FOREST       -> Seq((WOOD, 0.4), (SUGAR_CANE, 0.4), (FRUITS, 0.2)),
    TROPICAL_SEASONAL_FOREST   -> Seq((WOOD, 0.4), (SUGAR_CANE, 0.5), (FRUITS, 0.1)),
    TEMPERATE_DESERT -> Seq((ORE,1.0)), SUB_TROPICAL_DESERT -> Seq((ORE,1.0)),
    MANGROVE -> Seq((WOOD, 0.6), (FLOWER, 0.4)),
    TEMPERATE_RAIN_FOREST -> Seq((WOOD,0.8),(FUR, 0.2)),
    TAIGA -> Seq((WOOD, 1.0)),
    ALPINE -> Seq((ORE, 0.2), (FLOWER, 0.05))
  )

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
    if (res.isEmpty || res.keys.max < 1.0) res + (1.0 -> None) else res
  }

  /**
   * Return the resource to be associated to a given biome, using a given random generator
   * @param biome the biome to be mapped to a resource
   * @param rand the random generator to be used
   * @return a Resource
   */
  def apply(biome: Biome, rand: Random = new Random()): Resource = {
    val thresholds = propagate(_bindings.getOrElse(biome, Seq()))
    val value = rand.nextDouble()
    val result = thresholds((thresholds.keys filter { _ > value }).min)
    result
  }

}

