package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import eu.ace_design.island.map.ExistingBiomes._

/**
 * This process relies on the map elevation and moisture to assign a biome to each face.
 *
 * Land faces are assigned thanks to their elevation (using the one defined for their center) and moisture.
 *
 * Water faces with kind "Ocean" faces are always assigned as Oceans.
 * Water faces identified as Lakes can be a "lake", or a glacier (with respect to the elevation of their center)
 *
 * Pre-conditions:
 *   - Faces are identified with WaterKind({OCEAN, LAKE}), IsWater and HasForMoisture
 *   - Vertices are identified with HasForHeight
 *
 * Post-conditions:
 *   - All faces in the map are annotated with HasForBiome
 *
 * @param distribution The Whittaker diagram to be used to assign biomes for land and faces
 */
case class AssignBiomes(distribution: WhittakerDiagram = WhittakerDiagrams.complete) extends Process {

  def apply(m: IslandMap): IslandMap = {
    // Computing relevant data to help the biome assignment process
    val vertexElevations = m.vertexProps.restrictedTo(HasForHeight())
    // A vertex in the ocean does not have an altitude => using getOrElse
    val elevations = (m.faces map { f => m.faceRef(f) -> vertexElevations.getOrElse(f.center, 0.0) }).toMap
    val moistures  = m.faceProps.restrictedTo(HasForMoisture())
    val oceanRefs  = m.findFacesWith(Set(WaterKind(ExistingWaterKind.OCEAN))) map { m.faceRef }
    val lakeRefs   = m.findFacesWith(Set(WaterKind(ExistingWaterKind.LAKE)))  map { m.faceRef }
    val landRefs   = m.findFacesWith(Set(!IsWater())) map { m.faceRef }

    info("Assigning biomes to faces defined in the map")
    val oceans = (oceanRefs map { o => o -> OCEAN }).toMap                                  // oceans are oceans
    val lakes  = (lakeRefs  map { l => l -> distribution.freshWater(elevations(l)) }).toMap // lakes: glacier or lake
    val lands  = (landRefs  map { l => l -> distribution(moistures(l), elevations(l)) }).toMap
    val biomes = oceans ++ lakes ++ lands

    info("Updating the map")
    val fProps = (m.faceProps /: biomes) { case (acc, (ref, biome)) => acc + (ref -> HasForBiome(biome)) }
    m.copy(faceProps = fProps)
  }
}

/**
 * In the literature, a Whittaker diagram is used to distribute the different biomes with respect to temperature and
 * precipitation level. We use here a gross approximation, considering that the elevation is an approximation of the
 * temperature (higher elevations are colder than lowest one), and the soil moisture for the precipitation level (as
 * there is no precipitation concept in the Island's model)
 *
 * The diagram defines 2 operations:
 *   - for water faces, freshWater is used to decide if a fresh water reservoir is a lake or a glacier
 *   - for land faces,  apply takes as input an elevation and a moisture, and returns the associated biome.
 *
 * A diagram is not designed to address ocean faces.
 *
 */
trait WhittakerDiagram {
  import ExistingBiomes.{GLACIER, LAKE}

  /**
   * Compute the biome for a land faces
   * @param moisture moisture level of the face
   * @param elevation elevation of the face
   * @return on of the ExistingBiomes to be associated to this face (excepting GLACIER, OCEAN and LAKE)
   */
  def apply(moisture: Double, elevation: Double): Biome = {
    require(moisture >= 0.0 && moisture <= 100.0, "Moisture level must be in [0,100]")
    require(elevation >= 0.0, "Elevation level cannot be negative")
    assign(moisture, elevation)
  }

  /**
   * Compute the biome for a freshwater face
   * @param elevation elevation of the face
   * @return GLACIER or LAKE w.r.t. the elevation
   */
  def freshWater(elevation: Double): Biome = if (elevation >= iceLevel) GLACIER else LAKE

  // define the elevation where freshwater becomes a glacier
  protected val iceLevel: Double

  // implementation of the whittaker diagram, as a function.
  protected def assign(moisture: Double, elevation: Double): Biome

}

/**
 * a Library containing off-the-shelf Whittaker diagrams
 */
object WhittakerDiagrams {
  import ExistingBiomes._

  /**
   * The complete diagrams exhibits all the available biomes. It does not produce realist island (demo purpose)
   */
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

