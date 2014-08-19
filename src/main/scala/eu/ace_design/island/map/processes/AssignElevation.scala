package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, EdgeRegistry}
import eu.ace_design.island.map._


object ElevationFunctions {

  type ElevationFunction =  Map[Int,Double] => Map[Int,Double]

  def identity: ElevationFunction = m => m

  def peak(summit: Int): ElevationFunction = distances => {
    val max = distances.values.max
    distances map { case (key, distance) => key -> distance / max * summit }
  }

}


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
 *   - All vertices not in the ocean are tagged with "HasForHeight(h)", where h is the elevation. Not being tagged mean
 *     to be at the sea level by default (HasForHeight(0.0))
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
    val lakeFaces = m.faceProps.project(m.mesh.faces)(Set(WaterKind(ExistingWaterKind.LAKE)))
    val adjustment = adjust(lakeFaces, m, cornersElevation)

    info("Updating the property sets")
    val elevations = cornersElevation ++ centersElevation  ++ adjustment // the final elevations to be used
    val props = (m.vertexProps /: elevations) { (acc, pair) => acc + (pair._1 -> HasForHeight(pair._2)) }
    m.copy(vertexProps = props)
  }


  /**
   * The adjustment function for inner lakes. It takes as input the set of lakes, and returns a map where vertices
   * involved in an inner lake is bound to a more "normal" elevation (lakes are supposed to be flat ...)
   * @param lakes the set of faces identified as lakes in the current map
   * @param m the Island map
   * @return the adjustment map to be used to flatten the lakes.
   */
  private def adjust(lakes: Set[Face], m: IslandMap, elevations: Map[Int, Double]): Map[Int, Double] = {
    val clusters: Set[Set[Face]] = buildClusters(lakes, m)
    val adjustments = clusters map { setToMinHeight(_, m, elevations) }
    (Map[Int,Double]() /: adjustments) { (acc, map) => acc ++ map }
  }

  /**
   * Build clusters of the "inputs" lake set, identifying the different lakes that might exists inside an island
   * @param inputs
   * @param m
   * @return
   */
  private def buildClusters(inputs: Set[Face], m: IslandMap): Set[Set[Face]] = {
    def loop(ins: Set[Face], acc: Set[Set[Face]]): Set[Set[Face]] = ins.headOption match {
      case None => acc   // no more work to be done, return the accumulator
      case Some(face) => acc.flatten contains face match { // is this particular face already handled?
        case true  => loop(ins.tail, acc)  // yes, so move on to the next one
        case false => { // No, this face is not already handled => handle it and move to the next one!
          val faceRef = m.mesh.faces(face).get
          val surroundingLake = getLake(faceRef, m)
          val involvedFaces = surroundingLake map { ref => m.mesh.faces(ref) }
            loop(ins.tail, acc + involvedFaces)
        }
      }
    }
    loop(inputs, Set()) // starting the internal cluster building function
  }

  private def getLake(f: Int, m: IslandMap): Set[Int] = {
    def loop(faces: Set[Int], acc: Set[Int]): Set[Int] = faces.headOption match {
      case None => acc
      case Some(ref) => {
        val face = m.mesh.faces(ref)
        val relevant = face.neighbors.get filter { fRef =>
          m.faceProps.check(fRef, WaterKind(ExistingWaterKind.LAKE)) && !(acc contains fRef)
        }
        loop(faces.tail ++ relevant, acc + ref)
      }
    }
    loop(Set(f), Set())
  }

  private def setToMinHeight(lake: Set[Face], m: IslandMap, existing: Map[Int, Double]): Map[Int, Double] = {
    val vertices = lake flatMap { l => l.vertices(m.mesh.edges) }
    val minHeight = (vertices map { existing.getOrElse(_, Double.PositiveInfinity) }).min
    (vertices map { _ -> minHeight }).toMap
  }

}



