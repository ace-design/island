package eu.ace_design.island.stdlib

import eu.ace_design.island.stdlib.Colors._

/**
 * the different biomes available to build islands
 **/
object Biomes {

  import eu.ace_design.island.map.resources.Biome
  import Resources._

  val values: Set[Biome] = Set(ALPINE, SNOW, BEACH, TROPICAL_RAIN_FOREST, MANGROVE, TUNDRA, GRASSLAND,
    TROPICAL_SEASONAL_FOREST, TEMPERATE_DESERT, TAIGA, SUB_TROPICAL_DESERT,
    TEMPERATE_RAIN_FOREST, SHRUBLAND, TEMPERATE_DECIDUOUS_FOREST, OCEAN, LAKE, GLACIER)

  private val _bindings: Map[String, Biome] = (values map { biome => biome.code -> biome }).toMap
  /**
   * this function returns a biome from a 3-letter encoded key
   * @param key a string representing the biome
   * @return the associated biome, an exception elsewhere
   */
  def apply(key: String): Biome = _bindings(key)

  /************************************
   * Wet biomes: mangroves and snow  **
   ************************************/

  object MANGROVE extends Biome {
    override val code = "MAN"
    override protected val production = Seq((WOOD, 0.6), (FLOWER, 0.4))
    override val color = BROWN
  }

  object SNOW extends Biome {
    override val code = "SNO"
    override protected val production = Seq()
    override val color = WHITE
  }

  /*******************
   * Forest biomes  **
   *******************/

  object TROPICAL_RAIN_FOREST extends Biome {
    override val code = "trF"
    override protected val production = Seq((WOOD, 0.4), (SUGAR_CANE, 0.4), (FRUITS, 0.2))
    override val color = DARK_GREEN
  }

  object TROPICAL_SEASONAL_FOREST extends Biome {
    override val code = "trS"
    override protected val production = Seq((WOOD, 0.4), (SUGAR_CANE, 0.5), (FRUITS, 0.1))
    override val color = ULTRA_LIGHT_GREEN
  }

  object TAIGA extends Biome {
    override val code = "TAI"
    override protected val production = Seq((WOOD, 1.0))
    override val color = ULTRA_DARK_GREEN
  }

  object TEMPERATE_RAIN_FOREST extends Biome {
    override val code = "teR"
    override protected val production = Seq((WOOD,0.8),(FUR, 0.2))
    override val color = LIGHT_GREEN
  }

  object TEMPERATE_DECIDUOUS_FOREST extends Biome {
    override val code = "teF"
    override protected val production = Seq((WOOD, 1.0))
    override val color = MEDIUM_GREEN
  }

  /*****************************************************
   * "Prairie" biomes: grassland, shrubland and tundra *
   *****************************************************/

  object GRASSLAND extends Biome {
    override val code = "GRA"
    override protected val production = Seq((FUR, 1.0))
    override val color = LIGHT_ORANGE
  }

  object SHRUBLAND extends Biome {
    override val code = "SHR"
    override protected val production = Seq((FUR, 1.0))
    override val color = MEDIUM_ORANGE
  }

  object TUNDRA extends Biome {
    override val code = "TUN"
    override protected val production = Seq((FUR, 1.0))
    override val color = DARK_ORANGE
  }

  /***********************************************
   * Dry biomes: beach, deserts and alpine rocks *
   ***********************************************/

  object ALPINE extends Biome {
    override val code = "ALP"
    override protected val production = Seq((ORE, 0.2), (FLOWER, 0.05))
    override val color = DARK_GREY
  }

  object BEACH extends Biome {
    override val code = "BEA"
    override protected val production = Seq((FISH, 0.4))
    override val color = LIGHT_YELLOW
  }

  object SUB_TROPICAL_DESERT extends Biome {
    override val code = "STD"
    override protected val production = Seq((ORE,0.2), (QUARTZ, 0.4))
    override val color = MEDIUM_YELLOW
  }

  object TEMPERATE_DESERT extends Biome {
    override val code = "teD"
    override protected val production = Seq((ORE,0.3), (QUARTZ, 0.3))
    override val color = DARK_YELLOW
  }

  /****************
   * Water faces **
   ****************/

  object OCEAN extends Biome {
    override val code = "OCE"
    override protected val production = Seq((FISH,0.9))
    override val color = DARK_BLUE
  }

  object LAKE extends Biome {
    override val code = "LAK"
    override protected val production = Seq((FISH,0.8))
    override val color = MEDIUM_BLUE
  }

  object GLACIER extends Biome {
    override val code = "GLA"
    override protected val production = Seq((FLOWER, 0.05))
    override val color = LIGHT_BLUE
  }
}


