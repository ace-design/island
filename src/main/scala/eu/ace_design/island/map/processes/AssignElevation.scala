package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, EdgeRegistry}
import eu.ace_design.island.map._

/**
 * This object defines several elevation function used to assign the elevation (z coordinate) of each point
 */
object ElevationFunctions {

  // An elevation functions transforms a map of VertexRef -> DistanceToCoast into a map VertexRef -> Height
  type ElevationFunction =  Map[Int,Double] => Map[Int,Double]

  /**
   * The identity function consider that the elevation of a point is equivalent to its distance to the coastline
   * @return the elevation map
   */
  def identity: ElevationFunction = m => m

  /**
   * The peak function works like the identity one, and allows one to specify the elevation of the culminating point
   * @param summit the maximal elevation
   * @return the elevation map
   */
  def peak(summit: Int): ElevationFunction = distances => {
    val max = distances.values.max
    distances map { case (key, distance) => key -> distance / max * summit }
  }

  /**
   * This algorithm is used to redistribute the elevations according to the y = 1 - (1-x)^2 function.
   * (see http://www-cs-students.stanford.edu/~amitp/game-programming/polygon-map-generation/)
   *
   * The idea is to sort the points based on their distance to the coast. The redistribution function reshape the island
   * by defining the number of points having an elevation lesser than s given one. In this function, y is the number of
   * points handled, and x is the associated elevation. We rely on a normalized function, i.e., (x,y) \in [0,1]^2
   *
   * By sorting the distance map, we know the y value, assimilated to the position of the point in the sorted list. It
   * is then our responsibility to find the x, which maps to the elevation to be assigned to the associated points. I
   * must admit it looks like magic at first sight, but it is quite logic after all (Read Patel's blog post to see an
   * illustration).
   *
   * y = 1  - (1-x)^2
   *   = 2x - x^2
   * => - x^2 +2x - y = 0
   *
   * A y is known, this is a simple quadratic equation.  delta = 4 - 4y, and as y \in [0,1], delta > 0 \forall y
   * It admits 2 solutions :
   * s = (2 ± sqrt(4 - 4y) ) / 2  = (2 ± sqrt(4(1 - y)) ) / 2
   *   = (2 ± 2.sqrt(1 - y)) / 2  = 2 (1 ± sqrt(1 - y)) / 2
   *   = (1 ± sqrt(1 - y))        As (x,y) \in [0,1]^2, the only solution is (1 - sqrt(1 - y))
   *
   * Like Patel, we use a glitch (replacing 1 by 1.1) in s to strengthen the slopes a little bit.
   *
   * @param factor a factor used to rescale the slopes of the island
   * @return the elevation map
   */
  def redistribute(factor: Double = 1): ElevationFunction = distances => {
    val max = distances.values.max * factor; val sorted = distances.toSeq.sortBy(_._2)
    val yMax = sorted.length
    val redistributed = for(index <- 0 until yMax)
      yield {
        val y = index.toDouble / yMax ; val x = 1.1 - math.sqrt(1.1-y)
        sorted(index)._1 -> max * (if (x >=1) 1 else x)
      }
    redistributed.toMap
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

    info("Computing faces' center elevations as the average of the involved vertices' elevation")
    val land = m.faceProps.project(m.mesh.faces)(Set(!IsWater()))
    val centersElevation: Map[Int, Double] = (land map { face =>
      val corners = face.vertices(m.mesh.edges)
      val sum = (0.0 /: corners) { (acc, ref) => acc + cornersElevation(ref) }
      face.center -> sum / corners.size
    }).toMap

    info("Adjusting inner lakes to make them flat")
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
   * @param inputs the set of faces identified as a lake in the island
   * @param m the map
   * @return a set of lakes (i.e., set of faces involved in a given lake)
   */
  private def buildClusters(inputs: Set[Face], m: IslandMap): Set[Set[Face]] = {
    // recursive function to properly build the cluster
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

  /**
   * For a given face reference, identify its surrounding lake, i.e., its lake neighbors (transitive closure)
   * @param f the face reference to investigate
   * @param m the map to use
   * @return a set of face references (including f) that are part of the same lake (connected together)
   */
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

  /**
   * Put the elevation of the vertices involved in its given faces to the minimal one.
   * @param lake the set of face identified as a lake
   * @param m the map to be used to find the vertices involved in the faces (through the edges)
   * @param existing the existing elevation for the vertices that are not in lakes
   * @return a map binding each vertices involved in the lake (as corner) to the minimal elevation of this lake
   */
  private def setToMinHeight(lake: Set[Face], m: IslandMap, existing: Map[Int, Double]): Map[Int, Double] = {
    val verts = lake flatMap { l => l.vertices(m.mesh.edges) }
    val minHeight = (verts map { existing.getOrElse(_, Double.PositiveInfinity) }).min // the else should never happen
    (verts map { _ -> minHeight }).toMap
  }

}



