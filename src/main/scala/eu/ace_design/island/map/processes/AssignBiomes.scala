package eu.ace_design.island.map.processes

import eu.ace_design.island.map.ExistingBiomes._

/**
 * This process relies on the map elevation and moisture to assign a biome to each face.
 *
 * Land faces are assigned thanks to their elevation (using the one defined for their center) and moisture.
 *
 * Water faces with kind "Ocean" faces are always assigned as Oceans.
 * Water faces identified as Lakes can be a "lake", or a glacier (with respect to the elevation of their center)
 *
 * * Pre-conditions:
 *   - Faces are identified as IsWater and HasForMoisture
 *   - Vertices are identified with HasForElevation
 *
 * Post-conditions:
 *   - All faces in the map are annotated with HasForBiome
 *
 * @param distribution The Whittaker diagram to be used to assign biomes for land and faces
 */
case class AssignBiomes(distribution: WhittakerDiagram = WhittakerDiagrams.complete) extends Process {



}



trait WhittakerDiagram {

  def apply(moisture: Double, elevation: Double): Biome = {
    require(moisture >= 0.0 && moisture <= 100.0, "Moisture level must be in [0,100]")
    require(elevation >= 0.0, "Elevation level cannot be negative")
    assign(moisture, elevation)
  }

  def freshWater(elevation: Double): Biome = if (elevation > iceLevel) GLACIER else LAKE

  protected val iceLevel: Double

  protected def assign(moisture: Double, elevation: Double): Biome

}

object WhittakerDiagrams {

  object complete extends WhittakerDiagram {

    override val iceLevel = 130.0

    override def assign(moisture: Double, elevation: Double): Biome = elevation match {
      case e if e < 2.5 => moisture match {            /** SEA LEVEL biomes [0m - 25m] **/
        case m if m < 50 => BEACH
        case m if m < 80 => TROPICAL_RAIN_FOREST
        case _           => MANGROVE
      }
      case e if e < 5 => moisture match {               /** LOW LEVEL biomes [25m - 300m] **/
        case m if m < 30 => SUB_TROPICAL_DESERT
        case m if m < 50 => TROPICAL_SEASONAL_FOREST
        case m if m < 90 => TROPICAL_RAIN_FOREST
        case _           => MANGROVE
      }
      case e if e < 10 => moisture match {
        case m if m < 20 => SUB_TROPICAL_DESERT
        case m if m < 30 => GRASSLAND
        case m if m < 70 => TROPICAL_SEASONAL_FOREST
        case _           => TROPICAL_RAIN_FOREST
      }
      case e if e < 30 => moisture match {
        case m if m < 10 => SUB_TROPICAL_DESERT
        case m if m < 40 => GRASSLAND
        case m if m < 60 => TROPICAL_SEASONAL_FOREST
        case m if m < 70 => TEMPERATE_RAIN_FOREST
        case _           => TROPICAL_RAIN_FOREST
      }
      case e if e < 50 => moisture match {              /** HILL LEVEL biomes [300m - 900m] **/
        case m if m < 10 => TEMPERATE_DESERT
        case m if m < 40 => GRASSLAND
        case m if m < 60 => TROPICAL_SEASONAL_FOREST
        case _           => TEMPERATE_RAIN_FOREST
      }
      case e if e < 70 => moisture match {
        case m if m < 10 => TEMPERATE_DESERT
        case m if m < 40 => GRASSLAND
        case m if m < 80 => TEMPERATE_DECIDUOUS_FOREST
        case _           => TEMPERATE_RAIN_FOREST
      }
      case e if e < 90 => moisture match {
        case m if m < 20 => TEMPERATE_DESERT
        case m if m < 50 => GRASSLAND
        case m if m < 90 => TEMPERATE_DECIDUOUS_FOREST
        case _           => TEMPERATE_RAIN_FOREST
      }
      case e if e < 110 => moisture match {             /** MOUNTAIN LEVEL biomes [900m - 1700m] **/
        case m if m < 20 => TEMPERATE_DESERT
        case m if m < 40 => GRASSLAND
        case m if m < 50 => SHRUBLAND
        case _           => TEMPERATE_DECIDUOUS_FOREST
      }
      case e if e < 130 => moisture match {
        case m if m < 20 => TEMPERATE_DESERT
        case m if m < 60 => SHRUBLAND
        case m if m < 80 => TEMPERATE_DECIDUOUS_FOREST
        case _           => TAIGA
      }
      case e if e < 150 => moisture match {
        case m if m < 30 => TEMPERATE_DESERT
        case m if m < 60 => SHRUBLAND
        case m if m < 70 => TEMPERATE_DECIDUOUS_FOREST
        case _           => TAIGA
      }
      case e if e < 170 => moisture match {
        case m if m < 10 => TEMPERATE_DESERT
        case m if m < 30 => TUNDRA
        case m if m < 60 => SHRUBLAND
        case _           => TAIGA
      }
      case e if e < 190 => moisture match {             /** PEAK LEVEL biomes [1700m - 2100+ m] **/
        case m if m < 10 => ALPINE
        case m if m < 60 => TUNDRA
        case m if m < 90 => TAIGA
        case _           => SNOW
      }
      case e if e < 210 => moisture match {
        case m if m < 20 => ALPINE
        case m if m < 60 => TUNDRA
        case m if m < 80 => TAIGA
        case _           => SNOW
      }
      case _            => moisture match {
        case m if m < 50 => ALPINE
        case _           => SNOW
      }
    }
  }
}

