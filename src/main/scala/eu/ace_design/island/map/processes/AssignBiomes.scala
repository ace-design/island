package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import eu.ace_design.island.map.resources._
import eu.ace_design.island.stdlib.{WhittakerDiagrams, ExistingBiomes}
import ExistingBiomes._

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

    info("Assigning biomes to the faces defined in the map")
    val oceans = (oceanRefs map { o => o -> OCEAN }).toMap                                  // oceans are oceans
    val lakes  = (lakeRefs  map { l => l -> distribution.freshWater(elevations(l)) }).toMap // lakes: glacier or lake
    val lands  = (landRefs  map { l => l -> distribution(moistures(l), elevations(l)) }).toMap
    val biomes = oceans ++ lakes ++ lands

    info("Updating the map")
    val fProps = (m.faceProps /: biomes) { case (acc, (ref, biome)) => acc + (ref -> HasForBiome(biome)) }
    m.copy(faceProps = fProps)
  }
}



