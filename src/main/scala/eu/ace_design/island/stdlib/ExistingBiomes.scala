package eu.ace_design.island.stdlib

/**
 * the different biomes availabel to build islands
 **/
object ExistingBiomes extends Enumeration {

  type Biome = Value

  val  ALPINE, SNOW, BEACH, TROPICAL_RAIN_FOREST, MANGROVE, TUNDRA, GRASSLAND, TROPICAL_SEASONAL_FOREST,
  TEMPERATE_DESERT, TAIGA, SUB_TROPICAL_DESERT, TEMPERATE_RAIN_FOREST, SHRUBLAND, TEMPERATE_DECIDUOUS_FOREST,
  OCEAN, LAKE, GLACIER = Value

  /**
   * this function returns a biome from a 3-letter encoded key
   * @param key a string representing the biome
   * @return the associated biome, an exception elsewhere
   */
  def apply(key: String): Value = _binding(key)

  private val _binding: Map[String, Value] = Map(
    "ALP" -> ALPINE,
    "SNO" -> SNOW,
    "BEA" -> BEACH,
    "MAN" -> MANGROVE,
    "TUN" -> TUNDRA,
    "GRA" -> GRASSLAND,
    "TAI" -> TAIGA,
    "SHR" -> SHRUBLAND,
    "OCE" -> OCEAN,
    "LAK" -> LAKE,
    "GLA" -> GLACIER,
    "STD" -> SUB_TROPICAL_DESERT,
    "trF" -> TROPICAL_RAIN_FOREST,
    "trS" -> TROPICAL_SEASONAL_FOREST,
    "teR" -> TEMPERATE_RAIN_FOREST,
    "teD" -> TEMPERATE_DESERT,
    "teF" -> TEMPERATE_DECIDUOUS_FOREST
  )

}
