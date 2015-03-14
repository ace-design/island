package eu.ace_design.island.map.resources

import scala.util.Random


/**
  * A Resource, available on tiles for harvest purpose
  */
trait Resource extends eu.ace_design.island.util.NameAsClassName {
  override def toString = name
}

/**
 * A primary resource is produced by a Biome.
 */
trait PrimaryResource extends Resource  {

  // The amount of resources (in arbitrary unit) produced by one hectare (10,000 m2) of this biome
  val perHectare: Int

  // The extraction difficulty multiplicative factor
  val difficulty: Double

  /**
   * The amount of resource available on a given face depends on the area, and the typeof soil. A random generator is
   * used to introduce fluctuations
   * @param area the area of the face that is bound to this resource
   * @param soil the kind of soil (FERTILE, NORMAL, POOR), impacting the amount of resource available
   * @param rand a random generator, default is to create a new one
   * @return an integer value representing the amount of resource available
   */
  def amount(area: Double, soil: Option[Soils.Soil], rand: Random = new Random()): Int = {
    val s = soil.getOrElse(Soils.NORMAL)
    val theoretical = (area / 10000.0) * perHectare  // 1ha = 10,000 m2
    (theoretical * fluctuation(rand) * soil2factor(s)).toInt
  }

  /**
   * The extraction factor to be used to extract resource from this face.
   * @param pitch the pitch of the face
   * @param condition the exploitation condition (EASY, FAIR, HARSH)
   * @param rand  a random generator, default is to create a new one
   * @return
   */
  def extraction(pitch: Double, condition: Option[Conditions.Condition], rand: Random = new Random()): Double = {
    val c = condition.getOrElse(Conditions.FAIR)
    val factor = pitch match {
      case x if x < 10.0  => 1.2   // angle \in [0,  10[ degrees
      case x if x < 40.0  => 1.0   // angle \in [10, 21] degrees
      case x if x < 100.0 => 0.8   // angle \in [21, 45[ degrees
      case _ => 0.4                // angle >= 45 degrees
    }
    factor * fluctuation(rand) * condition2factor(c)
  }

  /**
   * Extract resources based on the number of workers available and the extraction factor
   * @param workers number of workers used to extract resources
   * @param factor the extraction factor to be used
   * @param rand a random generator, default is to create a new one
   * @return a couple of integers (collected, cost), where collected is the amount of resource collected ans cost
   */
  def harvest(workers: Int, factor: Double, rand: Random = new Random()): (Int, Int) = ???

  /**
   * Transform a soil into a multiplicative factor to be applied
   * @param s
   * @return
   */
  private def soil2factor(s: Soils.Soil): Double = s match {
    case Soils.FERTILE => 1.6
    case Soils.NORMAL  => 1.0
    case Soils.POOR    => 0.4
  }

  /**
   * transform a soil condition into a double factor
   * @param c
   * @return
   */
  private def condition2factor(c: Conditions.Condition): Double = c match {
    case Conditions.EASY  => 1.2
    case Conditions.FAIR  => 1.0
    case Conditions.HARSH => 0.7
  }

  /**
   * Introduce random fluctuation in the theoretical productions, in [0.9, 1.1]
   * @param rand
   * @return
   */
  private def fluctuation(rand: Random): Double = {
    ((rand.nextDouble() + rand.nextDouble()) / 10) + 0.9
  }

}

/**
 * This object represents the lack of resources
 */
object NoResource extends PrimaryResource {
  override val perHectare = 0; override val difficulty = 0.0
}

trait ManufacturedResource extends Resource
