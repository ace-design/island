package eu.ace_design.island.map

import eu.ace_design.island.map.resources.ExistingBiomes

/**
 * A property bound a value to an immutable key.  The trait is sealed, and cannot be implemented outside of this file.
 * @tparam T the type of value
 */
sealed trait Property[T] {
  // The key associated to this property
  val key: String
  // the value (a T)
  val value: T
}

/*********************************************
 ** Properties available in the Island game **
 *********************************************/

case class IsBorder(override val value: Boolean = true) extends Property[Boolean] {
  override val key = "isBorder"
  def unary_!() = IsBorder(value = ! this.value)
}

case class IsWater(override val value: Boolean = true) extends Property[Boolean] {
  override val key = "isWater"
  def unary_!() = IsWater(value = ! this.value)
}

case class WaterKind(override val value: ExistingWaterKind.ExistingWaterKind)
  extends Property[ExistingWaterKind.ExistingWaterKind] {
  override val key = "waterKind"
}

object ExistingWaterKind extends Enumeration {
  type ExistingWaterKind = Value
  val OCEAN, LAKE = Value
}

case class IsCoast(override val value: Boolean = true) extends Property[Boolean] {
  override val key = "isCoast"
  def unary_!() = IsCoast(value = ! this.value)
}

case class DistanceToCoast(override val value: Double  = 0.0) extends Property[Double] {
  override val key = "distanceToCoast"
}
case class HasForHeight(override val value: Double = 0.0) extends Property[Double] {
  override val key = "height"
}

case class RiverFlow(override val value: Int = 1) extends Property[Int] {
  override val key = "riverFlow"
}

case class HasForMoisture(override val value: Double = 0) extends Property[Double] {
  override val key = "moisture"
}

case class HasForBiome(override val value: ExistingBiomes.Biome = ExistingBiomes.ALPINE)
  extends Property[ExistingBiomes.Biome] { override val key = "biome" }


case class HasForArea(override val value: Double = 0.0) extends Property[Double] {
  override val key = "area"
}

case class HasForPitch(override val value: Double = 0.0) extends Property[Double] {
  override val key = "pitch"
}