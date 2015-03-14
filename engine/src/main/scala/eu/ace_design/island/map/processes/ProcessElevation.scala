package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, EdgeRegistry}
import eu.ace_design.island.map._
import eu.ace_design.island.util.Polynomial


/**
 * This trait defines functions to classically handle elevation in a given map (e.g., making lake flats)
 */
trait ElevationProcess extends Process {


  protected def buildVertexElevations(vertices: Set[Int], m: IslandMap): Map[Int, Double]

  override def apply(m: IslandMap): IslandMap = {
    info("Identifying corners for emerged lands area")
    val lakes = m.findFacesWith(Set(WaterKind(ExistingWaterKind.LAKE))) flatMap { f => m.cornerRefs(f)  }
    val lands = m.findFacesWith(Set(!IsWater())) flatMap { f => m.cornerRefs(f)  }
    val emerged = lakes ++ lands // everything that is not in the ocean can be elevated.

    info("Assigning initial elevation for emerged vertices")
    // vertices are mapped to a double value that is used for sorting, and the ordered sequence of vertices is returned
    val elevated = buildVertexElevations(emerged, m)

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
  protected def adjust(lakes: Set[Face], m: IslandMap, elevations: Map[Int, Double]): Map[Int, Double] = {
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
 * This process uses a mapping function to bind each emerged vertex to a double value. This sequence is sorted, and an
 * elevator function is used to map to each vertex reference an elevation. This leads to more irregular shapes, and
 * the island looks less "mathematical". The drawback is that it shortens the rivers for flat islands, as it is more
 * difficult to find a down slope path (and the generateRiver algorithm is naive and does not implement backtracking) to
 * the coast after the redistribution if the island is too flat.
 *
 * This process requires as pre-condition that the properties used in the mapper are available in the map.
 */
case class DistributeElevation(mapper: ElevationMappers.Mapper = ElevationMappers.distance,
                           elevator: ElevationDistributions.Distribution) extends ElevationProcess {

  override def buildVertexElevations(vertices: Set[Int], m: IslandMap): Map[Int, Double] = {
    val mapped = (vertices map { vertex => mapper(vertex,m) }).toSeq.sortBy{ _._2 } map { _._1 }
    elevator(mapped)
  }

}

/**
 * This process uses a mapper to compute a reference value, and then use an elevation function applied to each
 * reference value to assign a given height to each point. This leads to a shape which is purely controlled by the
 * function used as parameter, which ensure (if strictly decreasing) a way from a river source to the coast.
 *
 * The immediate drawback is the mathematical look the island have.
 *
 */
case class AssignElevation(mapper: ElevationMappers.Mapper = ElevationMappers.distance,
                           phi: ElevationFunctions.Function) extends ElevationProcess {

  override def buildVertexElevations(vertices: Set[Int], m: IslandMap): Map[Int, Double] = {
    val mapped = (vertices map { vertex => mapper(vertex, m) }).toMap
    phi(mapped)
  }

}

object ElevationFunctions {

  type Function = Map[Int,Double] => Map[Int,Double]

  def linear(highest: Double): Function = applyPolynomial(ElevationPolynomials.yEqualsToX)(highest)

  def plateau(highest: Double): Function = applyPolynomial(ElevationPolynomials.plateau)(highest)

  /**
   * Apply a polynomial function to a value, mapping valued obtained from a mapper to an elevation
   * @param phi the elevation function to apply
   * @param highest the highest elevation to produce
   * @param values the values obtained from a mapper function
   * @return the elevations to assign to each vertices
   */
  def applyPolynomial(phi: Double => Double)(highest: Double)(values: Map[Int,Double]): Map[Int,Double] = {
    val max = values.values.max
    values map { case (ref, value) => ref -> (value * phi(value/max) / max * highest) }
  }
}

/**
 * A mapper is used to bind each vertex to a reference value, to be used to compute the elevation. Elevation can be
 * defined through a mapping to the "minimal distance to coast", or based on the geographical location of the vertex
 */
object ElevationMappers {

  /**
   * a distribution mapper consumes a point reference (of a land vertex) and an IslandMap to produce a double. This double
   * is used to produce the ordered set to be used by the elevation function
   */
  type Mapper = (Int, IslandMap) => (Int, Double)

  val distance:    Mapper = (pRef, m) => pRef -> m.vertexProps.getValue(pRef, DistanceToCoast())
  val west2east:   Mapper = (pRef, m) => pRef -> m.vertex(pRef).x
  val north2south: Mapper = (pRef, m) => pRef -> m.vertex(pRef).y
}

/**
 * This object defines several elevation function used to assign the elevation (z coordinate) of each point
 */
object ElevationDistributions {

  /**
   * An elevation function consumes an ordered set of point references and map each point to an altitude
   */
  type Distribution = Seq[Int] => Map[Int, Double]

  def linear(highest: Double): Distribution = applyPolynomial(ElevationPolynomials.yEqualsToX)(highest)
  def flat(highest: Double): Distribution = applyPolynomial(ElevationPolynomials.plateau)(highest)

  /**
   * This function applies a given polynomial to an ordered sequence of vertices, following a distribution process. For
   * each vertex, we normalize its position in the sequence and then use the distribution function to compute the
   * new value.
   *
   * @param phi the distribution function
   * @param highest  the highest elevation to produce (for the last point of the sequence)
   * @param vertices the ordered sequence of vertices to be re-distributed.
   * @return the elevation according to the distrbution function
   */
  private def applyPolynomial(phi: Double => Double)(highest: Double)(vertices: Seq[Int]): Map[Int, Double] = {
    val length: Double = vertices.length.toDouble
    val raw = 0 until vertices.size map { index =>
      val normalized_x: Double = index / length
      val normalised_y: Double = phi(normalized_x)
      vertices(index) -> highest * normalised_y
    }
    raw.toMap
  }
}


/**
 * We consider here normalized polynomial function. We are only interested in values in [0,1], for both x and y
 * coordinates
 */
object ElevationPolynomials {

  def yEqualsToX = Polynomial(Seq(0,1))   // y = x

  def plateau = Polynomial(Seq(-0.0016, 0.7074, 9.1905, -68.2842, 174.3621, -187.6885, 72.7124))
}

