package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import eu.ace_design.island.geom.Edge
import scala.util.Random

/**
 * The process is used to create rivers in the island. A river is defined on the edges, and goes down from its source
 * to the coastline through the edges of the map.
 *
 * If the river is stuck (e.g., cannot go down), it simply run dry before reaching the coastline.
 *
 * If two (or more) river merge by sharing the same edge, it increases the flow for this edge.
 *
 * Pre-conditions:
 *   - Vertices are identified as IsWater(b), IsCoast(b) (b in {true, false}) and HasForHeight(x)
 *
 * Post-conditions:
 *   - Edges involved in rivers are identified as RiverFlow(n), n >= 1 representing its flow
 *
 * @param sources the number of rivers' sources to generate on the island (default to 10)
 * @param distance the minimal distance to coast (as a percentage in [0,1]) to be a source (default to 0)
 */
case class GenerateRivers(sources: Int = 10, distance: Double = 0.5) extends RandomizedProcess {
  require(sources >= 0, "Sources cannot be negative")
  require(distance >= 0 && distance <= 1, "distance must be in [0,1]")

  def apply(rand: Random)(m: IslandMap): IslandMap = {
    info("Identifying sources for rivers, flowing water from source to coast")
    val rivers = identifySources(rand, m) map { source => createRiver(source, m, rand) }
    // concatenating the rivers, grouping by shared edges and counting the numbers of rivers per shared edge.
    val raw = (Seq[Int]() /: rivers) { (acc, r) => acc ++ r} groupBy { n => n } map { case (k,v) => k -> v.size }
    // updating the property set for edges according tot he raw map computed before
    val props = (m.edgeProps /: raw) { (acc, pair) => acc + (pair._1 -> RiverFlow(pair._2))}
    m.copy(edgeProps = props)
  }

  /**
   * Identify the sources for the different rivers. A source is a vertex used a the corner of a face (as rivers flow
   * through edges), located on land and obviously not a coastal one.
   *
   * @param rand the random generator to use
   * @param m the map containing the vertices
   * @return a sequence of vertex to be used as river sources
   */
  private def identifySources(rand: Random, m: IslandMap): Seq[Int] = {
    // We are looking for vertices used as corners in faces, and annotated as land ones
    val avails =  (m.faces flatMap { m.cornerRefs }) & (m.findVerticesWith(Set(!IsWater())) map { m.vertexRef })
    // Among the available corners, we are looking for vertices located to a given distance from the coast
    val maxDist = m.vertexProps.restrictedTo(DistanceToCoast()).values.max
    val withDistance = avails filter { r => m.vertexProps.getValue(r, DistanceToCoast())/maxDist >= distance }
    val result = rand.shuffle(withDistance.toSeq).slice(0, sources)
    debug(s"Vertices used as sources: ${result.mkString("(", ", ", ")")}")
    result
  }

  /**
   * Create a River, starting a given vertex and based on the information stored in a map
   * @param source a vertex reference used to start the river
   * @param m the IslandMap to be used
   * @param rand a random generator to pick the next vertices when multiple candidates are available
   * @return a set of edge references representing the edges involved in this river.
   */
  private def createRiver(source: Int, m: IslandMap, rand: Random): Set[Int] = {
    debug(s"Creating a river, starting at #$source")
    def elevation(vRef: Int): Double = m.vertexProps.getValue(vRef, HasForHeight())
    def loop(from: Int, acc: Set[Int]): Set[Int] = m.vertexProps.check(from, IsCoast()) match {
      case true  => acc // coastline reached, and of the river
      case false =>
        val local = elevation(from)
        // building the set of crossed vertices, as the accumulator stores edge references and not vertex ones.
        val crossed = acc map { edgeRef => m.edge(edgeRef) } flatMap { e => Seq(e.p1, e.p2) }
        // Vertex candidates are less elevated that this one, and not already crossed (to get out of lakes)
        val candidates = m.neighbors(from) filter { elevation(_) <= local } diff crossed
        candidates.toSeq.sorted.headOption match {
          case None => acc // No more candidate available
          case Some(to) => loop(to, acc + m.edgeRef(Edge(from, to))) // finding one candidate
        }
    }
    val result = loop(source, Set())
    debug(s"River flows through edges ${ result.mkString("(", ", ", ")") }")
    result
  }

}
