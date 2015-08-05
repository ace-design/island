package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import eu.ace_design.island.map.resources.PIXEL_FACTOR

/**
 * The compute statistics process generate useful statistics about the map
 *
 * This process should be the last one applied to the map to benefit from the complete chain
 *
 * Pre-conditions:
 *   - Faces annotated by: HasForArea, WaterKind, IsWater
 *
 * Post-conditions:
 *   - The map "stats" slot is now filed.
 *
 */
object ComputeStatistics extends Process {
  import Statistics._

  override def apply(m: IslandMap): IslandMap = {
    val stats = computeAreas(m) ++ computeElevations(m) ++ computePitches(m)
    m.copy(stats = Some(stats))
  }

  /**
   * Make stats about faces areas (in hectares, 1ha == 10,000 m2, a square of 100m x 100m)
   *
   * FYI, here are some landmarks:
   *   - the Polytech'Nice campus is around 10ha.
   *
   *   - Sainte Marguerite: 2 km2 =>     200 ha     [http://en.wikipedia.org/wiki/%C3%8Ele_Sainte-Marguerite]
   *   - Porquerolles:     12 km2 =>   1,200 ha     [http://en.wikipedia.org/wiki/Porquerolles]
   *   - Key West:         19 km2 =>   1,900 ha     [http://en.wikipedia.org/wiki/Key_West,_Florida]
   *   - Saint Barthélemy: 24 km2 =>   2,400 ha     [http://en.wikipedia.org/wiki/Saint_Barth%C3%A9lemy]
   *   - Jersey:          118 km2 =>  11,800 ha     [http://en.wikipedia.org/wiki/Jersey]
   *   - Malta:           246 km2 =>  24,600 ha     [http://en.wikipedia.org/wiki/Malta_(island)]
   *   - Saint Vincent    346 km2 =>  34,600 ha     [http://en.wikipedia.org/wiki/Saint_Vincent_(island)]
   *   - Grenada:         348 km2 =>  34,800 ha     [http://en.wikipedia.org/wiki/Grenada]
   *   - Barbados:        432 km2 =>  43,200 ha     [http://en.wikipedia.org/wiki/Barbados]
   *   - Madeira:         740 km2 =>  74,000 ha     [http://en.wikipedia.org/wiki/Madeira]
   *   - Corsica:        8700 km2 => 870,000 ha     [http://en.wikipedia.org/wiki/Corsica]
   *
   * @param m
   * @return
   */
  private def computeAreas(m: IslandMap): Map[StatName, String] = {
    info("Computing statistics about faces' areas")
    def toHectares(m2: Double): Double = m2 / 10000 // 1ha == 10,000 m2

    val areas = m.faceProps.restrictedTo(HasForArea())
    val biomes = m.faceProps.restrictedTo(HasForBiome())

    val totalArea = toHectares((0.0 /: areas.values) { _ + _ })
    val oceansArea = toHectares((0.0 /: oceans(m)) { (acc, r) => acc + areas(r) })
    val landsArea =  toHectares((0.0 /: lands(m))  { (acc, r) => acc + areas(r) })
    val lakesArea =  toHectares((0.0 /: lakes(m))  { (acc, r) => acc + areas(r) })

    val result: Map[StatName, String] = Map(
      TOTAL_AREA -> f"$totalArea%2.2f", AVERAGE_AREA -> f"${totalArea / areas.size}%2.2f",
      LAKE_AREA -> f"$lakesArea%2.2f",  OCEAN_AREA -> f"$oceansArea%2.2f",
      LAND_AREA -> f"$landsArea%2.2f",  LAKE_PERCENTAGE -> f"${lakesArea/totalArea*100}%2.2f",
      LAND_PERCENTAGE -> f"${landsArea/totalArea*100}%2.2f", OCEAN_PERCENTAGE -> f"${oceansArea/totalArea*100}%2.2f"
    )
    debug(result.mkString("Map(",",",")"))
    result
  }


  /**
   * Compute statistics related to vertices elevations (min, max, avg)
   * @param m
   * @return
   */
  private def computeElevations(m: IslandMap): Map[StatName, String] = {
    info("Computing statistics about the elevation of the map (land faces)")
    val all = m.vertexProps.restrictedTo(HasForHeight()) ;
    val landVertexRefs = m.findVerticesWith(Set(!IsWater())) map { m.vertexRef(_) }
    val elevations = all filter { case (key, _) => landVertexRefs.contains(key) }
    val avg = (0.0 /: elevations.values) { _ + _ } / elevations.size
    Map(ELEVATION_MAX -> f"${elevations.values.max * PIXEL_FACTOR}%2.2f",
        ELEVATION_MIN -> f"${elevations.values.min * PIXEL_FACTOR}%2.2f",
        ELEVATION_AVG -> f"${avg * PIXEL_FACTOR}%2.2f")
  }

  /**
   * Compute statistics about faces pitches (in %)
   * @param m
   * @return
   */
  private def computePitches(m: IslandMap): Map[StatName, String] = {
    info("Computing statistics about lands pitches")
    val all = m.faceProps.restrictedTo(HasForPitch())
    val landsRefs = lands(m)
    val pitches = (all filter { case (key, _) => landsRefs.contains(key) }).values
    val avg = (0.0 /: pitches) { _ + _ } / pitches.size

    Map(PITCH_MAX -> f"${pitches.max}%2.2f", PITCH_MIN -> f"${pitches.min}%2.2f", PITCH_AVG -> f"$avg%2.2f")
  }

  import ExistingWaterKind._

  private def lands(m: IslandMap):  Set[Int] = m.findFacesWith(Set(!IsWater())) map { m.faceRef }
  private def lakes(m: IslandMap):  Set[Int] = m.findFacesWith(Set(WaterKind(LAKE))) map { m.faceRef }
  private def oceans(m: IslandMap): Set[Int] = m.findFacesWith(Set(WaterKind(OCEAN))) map { m.faceRef }
}

object Statistics extends Enumeration {
  type StatName = Value
  val TOTAL_AREA        = Value("Map area (ha)                  ")
  val AVERAGE_AREA      = Value("Face area (ha) [avg]           ")
  val LAND_AREA         = Value("Area occupied by lands (ha)    ")
  val LAKE_AREA         = Value("Area occupied by lakes (ha)    ")
  val OCEAN_AREA        = Value("Area occupied by the ocean (ha)")
  val LAND_PERCENTAGE   = Value("% of map occupied by lands     ")
  val OCEAN_PERCENTAGE  = Value("% of map occupied by the ocean ")
  val LAKE_PERCENTAGE   = Value("% of map occupied by lakes     ")
  val ELEVATION_MIN     = Value("Land elevation (m) [min]       ")
  val ELEVATION_MAX     = Value("Land elevation (m) [max]       ")
  val ELEVATION_AVG     = Value("Land elevation (m) [avg]       ")
  val PITCH_MIN         = Value("Pitch for lands (%) [min]      ")
  val PITCH_MAX         = Value("Pitch for lands (%) [max]      ")
  val PITCH_AVG         = Value("Pitch for lands (%) [avg]      ")

}

