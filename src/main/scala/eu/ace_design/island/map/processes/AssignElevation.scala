package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.EdgeRegistry
import eu.ace_design.island.map._


/**
 * This process assigns an elevation (z coordinate) to the vertices stored in the map, according to a given function.
 * The function (phi) used as parameter is applied to the distance to the coast of each corner. Contrarily to the
 * corners, vertices used as face' center has for elevation the average of the elevation of the vertices used in the
 * border of the face.
 *
 * Pre-conditions:
 *   - The map contains "DistanceToCoast(d)" annotations for each land (i.e., !ocean) vertices.
 *
 * Post-conditions:
 *   - All vertices are  tagged with "HasForHeight(h)", where h is the elevation.
 */
case class AssignElevation(phi: ElevationFunctions.ElevationFunction) extends Process {

  override def apply(m: IslandMap): IslandMap = {
    info("Assigning initial elevation for corner vertices")
    val corners =  m.mesh.vertices.references diff (m.mesh.faces.values map { _.center })
    val distances = (m.vertexProps restrictedTo DistanceToCoast()) filter { case (k,_) => corners contains k }
    val cornersElevation = phi(distances)

    info("Computing faces' center elevations")
    val land = m.faceProps.project(m.mesh.faces)(Set(!IsWater()))
    val centersElevation: Map[Int, Double] = (land map { face =>
      val corners = face.vertices(m.mesh.edges)
      val sum = (0.0 /: corners) { (acc, ref) => acc + cornersElevation(ref) }
      face.center -> sum / corners.size
    }).toMap

    info("Adjusting inner lakes to make it flat")
    val fProps =  m.faceProps.project(m.mesh.faces) _
    val lakes = fProps(Set(WaterKind(ExistingWaterKind.LAKE))) flatMap { f => f.vertices(m.mesh.edges) + f.center }
    val adjustment: Map[Int, Double] =  (lakes map { _ -> 400.0 }).toMap     // TODO FIX THIS

    info("Updating the property sets")
    val elevations = cornersElevation ++ centersElevation  ++ adjustment
    val updatedVertProps = (m.vertexProps /: elevations) { (acc, pair) => acc + (pair._1 -> HasForHeight(pair._2)) }
    //println(oceans)
    m.copy(vertexProps = updatedVertProps)
  }
}


object ElevationFunctions {

  type ElevationFunction =  Map[Int,Double] => Map[Int,Double]

  def identity: ElevationFunction = m => m

  def peak(summit: Int): ElevationFunction = distances => {
    val max = distances.values.max
    distances map { case (key, distance) => key -> distance / max * summit }
  }

}
