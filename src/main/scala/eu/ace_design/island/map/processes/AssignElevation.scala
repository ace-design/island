package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, EdgeRegistry}
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
 *   - All vertices not in the ocean are tagged with "HasForHeight(h)", where h is the elevation. Not being tagged mean
 *     to be at the sea level by default (HasForHeight(0.0))
 */
case class AssignElevation(mapper: ElevationFunctions.DistributionMapper = ElevationFunctions.distance,
                           elevator: ElevationFunctions.ElevationFunction) extends Process {

  override def apply(m: IslandMap): IslandMap = {
    info("Identifying emerged lands area")
    val oceans = m.findFacesWith(Set(WaterKind(ExistingWaterKind.OCEAN))) flatMap { f => m.cornerRefs(f) + f.center }
    val emerged = m.vertexRefs diff oceans // everything that is not in the ocean can be elevated.

    info("Assigning initial elevation for emerged vertices")
    // vertices are mapped to a double value that is used for sorting, and the ordered sequence of vertices is returned
    val mapped = (emerged map { vertex => vertex -> mapper(vertex,m) }).toSeq.sortBy{ _._2 } map { _._1 }
    val elevated = elevator(mapped)

    info("Computing faces' center elevations as the average of the involved vertices' elevation")
    val land = m.findFacesWith(Set(!IsWater()))
    val centersElevation: Map[Int, Double] = (land map { face =>
      val corners = m.cornerRefs(face)
      val sum = (0.0 /: corners) { (acc, ref) => acc + elevated(ref) }
      face.center -> sum / corners.size
    }).toMap

    info("Adjusting inner lakes to make them flat")
    val lakeFaces = m.findFacesWith(Set(WaterKind(ExistingWaterKind.LAKE)))
    val adjustment = adjust(lakeFaces, m, elevated)

    info("Updating the property sets")
    val elevations = elevated ++ centersElevation  ++ adjustment // the final elevations to be used
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
          val faceRef = m.faceRef(face)
          val surroundingLake = getLake(faceRef, m)
          val involvedFaces = surroundingLake map { ref => m.face(ref) }
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
        val face = m.face(ref)
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
    val verts = lake flatMap { face => m.cornerRefs(face) + face.center }
    val minHeight = (verts map { existing.getOrElse(_, Double.PositiveInfinity) }).min // the else should never happen
    (verts map { _ -> minHeight }).toMap
  }

}


/**
 * This object defines several elevation function used to assign the elevation (z coordinate) of each point
 */
object ElevationFunctions {

  /**
   * a distribution mapper consumes a point reference (of a land vertex) and an IslandMap to produce a double. This double is used to
   * produce the ordered set to be used by the elevation function
   */
  type DistributionMapper = (Int, IslandMap) => Double

  val distance:    DistributionMapper = (pRef, m) => m.vertexProps.getValue(pRef, DistanceToCoast())
  val west2east:   DistributionMapper = (pRef, m) => m.vertex(pRef).x
  val north2south: DistributionMapper = (pRef, m) => m.vertex(pRef).y


  /**
   * An elevation function consumes an ordered set of point references and map each point to an altitude
   */
  type ElevationFunction = Seq[Int] => Map[Int, Double]


  def linear(highest: Double)(vertices: Seq[Int]): Map[Int, Double] = {
    val phi: Double => Double = x => x
    val length: Double = vertices.length.toDouble
    val raw = 0 until vertices.size map { index =>
      val normalized_x: Double = index / length
      val normalised_y: Double = phi(normalized_x)
      vertices(index) -> highest * normalised_y
    }
    raw.toMap
  }


  def flat(highest: Double)(vertices: Seq[Int]): Map[Int, Double] = {
    val phi = polynomial(Seq(-0.0016, 0.7074, 9.1905, -68.2842, 174.3621, -187.6885, 72.7124)) _
    val length: Double = vertices.length.toDouble
    val raw = 0 until vertices.size map { index =>
      val normalized_x: Double = index / length
      val normalised_y: Double = phi(normalized_x)
      vertices(index) -> highest * normalised_y
    }
    raw.toMap
  }


  /**
   * Compute the evaluation of a given polynomial (a sequence of coefficient, from x**0 to x**n) and a given x
   * @param coefficients the ascending sequence of coefficients to apply
   * @param x the value
   * @return the value of this polynomial function for x
   */
  private def polynomial(coefficients: Seq[Double])(x: Double): Double = {
    (0.0 /: (0 until coefficients.size)) { (acc, i) => acc + math.pow(x,i) * coefficients(i) }
  }
}


