package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.Point
import eu.ace_design.island.map._

/**
 * This process is used to assign moisture to each vertex and face a moisture level. The moisture of a vertex is defined
 * by the sum of the moisture propagation function applied to each source of fresh water (i.e., lakes, rivers) located
 * at an higher elevation than this vertex (as water goes down thanks to Newton). This is kinda naive but should do the
 * trick.
 *
 * Obviously, a "big" river (RiverFlow(n), n "big") will more moisturize its surrounding vertices than a creek. This is
 * modelled thanks to a "flow" value associated to each vertices. For a vertex involved in a river, this is basically
 * the RiverFlow value. When involved in a lake, we use a LAKE_FACTOR constant.
 *
 * The moisture of a face is defined as the average of the moisture of its involved vertices (including center)
 *
 * Pre-conditions:
 *   - Edges are annotated with RiverFlow(n)
 *   - Vertices are annotated with HasForHeight(x) and IsWater()
 *   - Faces are annotated with WaterKind(LAKE)
 *
 * Post-conditions:
 *   - Faces and Vertices are annotated with HasForMoisture(x)
 *
 * @param propagation the propagation function to be used (ideally, but not restricted to) in MoisturePropagation
 * @param redistribution a redistribution function, if needed
 */
case class AssignMoisture(propagation: Double => Double = MoisturePropagation.order2,
                          redistribution: Map[Int, Double] => Map[Int, Double] = MoistureDistribution.identity)
  extends Process {

  final val LAKE_FACTOR: Int = 2

  override def apply(m: IslandMap): IslandMap = {
    info("Identifying vertices to be used as sources of fresh water")
    val sources = identifyFreshWater(m)

    info("Computing moisture for vertices")
    val landRefs = m.findVerticesWith(Set(!IsWater())) map { m.vertexRef }
    val elevations = m.vertexProps.restrictedTo(HasForHeight())
    val rawMoist = (landRefs map { vRef => vRef -> moisturize(vRef, m, sources, elevations) }).toMap
    val moistureMap = redistribution(rawMoist)
    val vProps = (m.vertexProps /: landRefs) { (acc, r) => acc + (r -> HasForMoisture(moistureMap(r)) )}

    info("Computing moisture for faces")
    val landFaceRefs = m.findFacesWith(Set(!IsWater())) map { m.faceRef }
    val faceMap = (landFaceRefs map { ref =>
      val f = m.face(ref)
      val vertices = m.cornerRefs(f) + f.center
      ref -> (0.0 /: vertices) { (acc, r) => acc + moistureMap(r) } / vertices.size
    }).toMap
    val fProps = (m.faceProps /: landFaceRefs) { (acc, r) => acc + (r -> HasForMoisture(faceMap(r))) }

    m.copy(vertexProps = vProps, faceProps = fProps)
  }

  /**
   * Identify sources of fresh water (i.e., vertex involved in lakes and rivers) in a given map
   * @param m the map to analyse
   * @return a map where vertex references (sources) are bound to a "flow" value ( flow >= 1 )
   */
  private def identifyFreshWater(m: IslandMap): Map[Int,Int] = {
    // Handling Lakes
    val lakes = m.findFacesWith(Set(WaterKind(ExistingWaterKind.LAKE))) flatMap { f => m.cornerRefs(f) + f.center }
    val lakesMap = (lakes map { _ -> LAKE_FACTOR }).toMap
    // Handling Rivers: finding all vertices involved in a river, associate it with its RiverFlow
    val riversMap = (m.edgeProps.restrictedTo(RiverFlow()).toSeq flatMap { case (k, v) =>
      val e = m.edge(k); Seq(e.p1 -> v, e.p2 -> v)
    }).toMap
    val result = merge(lakesMap, riversMap)
    debug(s" Identified sources: $result")
    result
  }

  /**
   * Merge 2 maps (vertexRef -> RiverFlow), keeping the maximal river flow if vertexRef exists in the 2 inputs
   * @param m1 the first map to merge
   * @param m2 the second map to merge
   * @return a map that contains all keys of m1 and m2, and where maximal value is kept in case of overlapping
   */
  private def merge(m1: Map[Int, Int], m2: Map[Int,Int]): Map[Int, Int] = {
    def loop(ins: Seq[(Int, Int)], acc: Map[Int, Int]): Map[Int, Int] = ins.headOption match {
      case None => acc
      case Some((vRef,flow)) => loop(ins.tail, acc + (vRef -> math.max(flow, acc.getOrElse(vRef, 0))))
    }
    loop(m2.toSeq, m1)
  }

  /**
   * Compute the moisture value fro a given vertex
   * @param vertexRef the reference of the vertex to be moisturized
   * @param m the island map (to match vertices's references to real vertices)
   * @param sources the sources of fresh water (vertex reference -> flow)
   * @param elevations the elevations of the different land vertices (as a vertex is moisturized by upstream vertices)
   * @return the moisture value for the given vertex
   */
  private def moisturize(vertexRef: Int, m: IslandMap, sources: Map[Int, Int], elevations: Map[Int, Double]): Double = {
    val p = m.vertex(vertexRef)
    val upstream = sources filter { case (k, _) => elevations(k) >= elevations(vertexRef) }
    val moisture = upstream map { case (ref, flow) =>  flow * MoisturePropagation(propagation)(p --> m.vertex(ref)) }
    (0.0 /: moisture) { (acc, m) => acc + m }
  }

}


object MoistureDistribution {
  val identity: Map[Int, Double] => Map[Int, Double] = m => m
}

/**
 * A moisture propagation function returns the moisture (in [0,100]) based on the distance between a point and the
 * fresh water source under consideration. We consider as an assumption that the propagation stops when the distance is
 * greater than 50 units (implemented in each function).
 *
 * The order changes the decrease rate according to the distance. Lower orders drop more quickly than higher ones.
 *
 */
object MoisturePropagation {

  val orderSqrt: Double => Double = x => -15 * math.sqrt(x) + 100   // sqrt(x) == x^(1/2)
  val order1: Double => Double = x => - 2 * x               + 100
  val order2: Double => Double = x => - (math.pow(x,2)/ 5)  + 100
  val order3: Double => Double = x => - (math.pow(x,3)/10)  + 100
  val order4: Double => Double = x => - (math.pow(x,4)/15)  + 100

  def apply(phi: Double => Double)(distance: Double): Double = {
    require(distance >= 0, "The distance cannot be negative")
    val y = phi(distance); if (y <= 0) 0 else y
  }

}
