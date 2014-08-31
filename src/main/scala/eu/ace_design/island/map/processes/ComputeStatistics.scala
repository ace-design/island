package eu.ace_design.island.map.processes

import eu.ace_design.island.map._

/**
 * The compute statistics process generate useful statistics about the map
 *
 * Pre-conditions:
 *   - This process should be the last one applied to the map to benefit from the complete chain
 *
 * Post-conditions:
 *   - The map "stats" slot is now filed.
 *
 */
object ComputeStatistics extends Process {
  import Statistics._

  override def apply(m: IslandMap): IslandMap = {
    val stats = computeAreas(m)
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
      LAKE_AREA -> f"$lakesArea%2.2f", OCEAN_AREA -> f"$oceansArea%2.2f", LAND_AREA -> f"$landsArea%2.2f",
      LAKE_PERCENTAGE -> f"${lakesArea/totalArea*100}%2.2f", LAND_PERCENTAGE -> f"${landsArea/totalArea*100}%2.2f",
      OCEAN_PERCENTAGE -> f"${oceansArea/totalArea*100}%2.2f"
    )
    info(result.mkString("Map(",",",")"))
    result
  }

  import ExistingWaterKind._

  private def lands(m: IslandMap):  Set[Int] = m.findFacesWith(Set(!IsWater())) map { m.faceRef }
  private def lakes(m: IslandMap):  Set[Int] = m.findFacesWith(Set(WaterKind(LAKE))) map { m.faceRef }
  private def oceans(m: IslandMap): Set[Int] = m.findFacesWith(Set(WaterKind(OCEAN))) map { m.faceRef }
}

object Statistics extends Enumeration {
  type StatName = Value
  val TOTAL_AREA, AVERAGE_AREA,
      LAND_AREA, LAKE_AREA, OCEAN_AREA,
      LAND_PERCENTAGE, OCEAN_PERCENTAGE, LAKE_PERCENTAGE = Value
}

