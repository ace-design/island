package eu.ace_design.island.io

import eu.ace_design.island.map._
import eu.ace_design.island.map.resources.{Biome, Soils, Conditions}
import eu.ace_design.island.stdlib.Biomes
import org.json.JSONObject

/**
 * This file is part of the island project
 * @author mosser (03/08/2015, 21:39)
 **/
object PropertySetFactory {




}


/**
 * Handle the serialization / deserialization of properties.
 */
object PropertyFactory {

  /**
   * Transform a JSON object into a property
   * @param obj
   * @return
   */
  def apply(obj: JSONObject): Property[_] = obj.getString("p") match {
      // Boolean properties
    case p if p == IsBorder().key        => IsBorder(obj.getBoolean("v"))
    case p if p == IsWater().key         => IsWater(obj.getBoolean("v"))
    case p if p == IsCoast().key         => IsCoast(obj.getBoolean("v"))
      // Int properties
    case p if p == RiverFlow().key       => RiverFlow(obj.getInt("v"))
      // Double properties
    case p if p == DistanceToCoast().key => DistanceToCoast(obj.getDouble("v"))
    case p if p == HasForHeight().key    => HasForHeight(obj.getDouble("v"))
    case p if p == HasForMoisture().key  => HasForMoisture(obj.getDouble("v"))
    case p if p == HasForArea().key      => HasForArea(obj.getDouble("v"))
    case p if p == HasForPitch().key     => HasForPitch(obj.getDouble("v"))
      // Enum-based properties
    case p if p == WaterKind().key       => WaterKind(ExistingWaterKind.withName(obj.getString("v")))
    case p if p == HasForCondition().key => HasForCondition(Conditions.withName(obj.getString("v")))
    case p if p == HasForSoil().key      => HasForSoil(Soils.withName(obj.getString("v")))
      // Stdlib based properties
    case p if p == HasForBiome().key     => HasForBiome(Biomes(obj.getString("v")))
      // Error
    case unknown => throw new IllegalArgumentException(s"Unknown property name [$unknown]")
  }

  /**
   * Transform a given property into its associated JSON representation
   * @param prop
   * @return
   */
  def apply(prop: Property[_]): JSONObject = {
    new JSONObject().put("p", prop.key).put("v", prop.value match {
      case b: Boolean => b
      case i: Int     => i
      case d: Double  => d
      case b: Biome   => b.code
      case other      => other.toString
    })
  }

  /**
   * Transform a string containing a JSON property into a Property
   * @param str
   * @return
   */
  def apply(str: String): Property[_] = this(new JSONObject(str))

}
