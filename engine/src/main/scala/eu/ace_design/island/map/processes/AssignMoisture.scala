package eu.ace_design.island.map.processes

import eu.ace_design.island.map.ExistingWaterKind
import eu.ace_design.island.map.ExistingWaterKind._
import eu.ace_design.island.map._

import scala.util.Random

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
 * Some faces contain underground aquifers, generated randomly. The vertices involves in an aquifer (half of the corners
 * and the face center) are considered as a source of fresh water, using the AQUIFER_FACTOR constant
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
 * @param aquifers the nunmber of underground aquifers to generate on this map.
 */
case class AssignMoisture(propagation: Int => Double => Double,
                          aquifers: Int = 10) extends RandomizedProcess {

  final val LAKE_FACTOR: Int    = 2
  final val AQUIFER_FACTOR: Int = 1

  override def apply(rand: Random)(m: IslandMap): IslandMap = {
    info("Identifying vertices to be used as sources of fresh water")
    val sources = merge(identifyFreshWater(m), createAquifers(rand, m))

    info("Computing moisture for vertices")
    val landRefs = m.findVerticesWith(Set(!IsWater())) map { m.vertexRef }
    val lakesRefs = m.findFacesWith(Set(WaterKind(LAKE))) flatMap { f => m.cornerRefs(f) + f.center }
    val vertices = landRefs ++ lakesRefs
    val elevations = m.vertexProps.restrictedTo(HasForHeight())
    val moistureMap = (vertices map { vRef => vRef -> moisturize(vRef, m, sources, elevations) }).toMap

    debug(s"Vertex: min_moist = ${moistureMap.values.min}, max_moist = ${moistureMap.values.max} ")

    val vProps = (m.vertexProps /: vertices) { (acc, r) => acc + (r -> HasForMoisture(moistureMap(r)) )}

    info("Computing moisture for faces")
    val landFaceRefs = m.findFacesWith(Set(!IsWater())) map { m.faceRef }
    val faceMap = (landFaceRefs map { ref =>
      val f = m.face(ref)
      val vertices = m.cornerRefs(f) + f.center
      ref -> (0.0 /: vertices) { (acc, r) => acc + moistureMap(r) } / vertices.size
    }).toMap
    debug(s"Faces: min_moist = ${faceMap.values.min}, max_moist = ${faceMap.values.max} ")
    val fProps = (m.faceProps /: landFaceRefs) { (acc, r) => acc + (r -> HasForMoisture(faceMap(r))) }

    m.copy(vertexProps = vProps, faceProps = fProps)
  }

  /**
   * Identify sources of fresh water (i.e., vertex involved in lakes and rivers) in a given map. The process introduces
   * random sources of underground water on the island
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
   * Create underground aquifers. An aquifer is located under a land face, and impacts a subset of the corners (50%) of
   * this face (+ its center).
   * @param rand the random generator to be used to locate the aquifers
   * @param m the map
   * @return a map of freshwater sources (vertexRef -> Flow)
   */
  private def createAquifers(rand: Random, m: IslandMap): Map[Int, Int] = {
    val selected = rand.shuffle(m.findFacesWith(Set(!IsWater())).toSeq).slice(0, aquifers)
    val vertices = selected flatMap { f =>
      val corners = m.cornerRefs(f).toSeq
      f.center +: rand.shuffle(corners).slice(0,corners.size/2)
    }
    (vertices map { _ -> AQUIFER_FACTOR}).toMap
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
    val point = m.vertex(vertexRef)
    val upstream = sources filter { case (k, _) => elevations(k) >= elevations(vertexRef) }
    val moisture = upstream map { case (ref, flow) =>  propagation(flow)(point --> m.vertex(ref)) } filter { _ > 0.0 }
    val result = sources.isDefinedAt(vertexRef) match {
      case true  => MoisturePropagation.MAX_MOISTURE
      case false => if (moisture.size == 0) 0 else (0.0 /: moisture) { (acc, m) => acc + m } / moisture.size
    }
    trace(s"$vertexRef => $result / $moisture")
    result
  }

}

/**
 * A moisture propagation function is a function defined on the distance from a point to a source of fresh water. It
 * returns a moisture value for the given distance, in [0, moistMax]. The more close the point is to the source of
 * water, the higher the value is. Distances greater than distMax are not impacted by this source of water.
 *
 * The library comes with 3 functions:
 *
 *   - the linear propagation follows the -x+1 function. It correspond to "normal" soil.
 *   - the wet propagation follows the -x**flow+1 function. It corresponds to a rich soil that keeps the moisture
 *   - the dry propagation follows the (-x+1)**flow function. It corresponds to a washed soil where moisture vanish
 *
 */
object MoisturePropagation {

  type MoistureFunction = Double => Double

  final val MAX_MOISTURE: Double = 100.0

  def wet(distMax: Int)(flow: Int)(dist: Double): Double = {
    if (dist > distMax) 0 else MAX_MOISTURE * ( -1 / math.pow(distMax,flow+1) * math.pow(dist,flow + 1) + 1)
  }

  def linear(distMax: Int)(flow: Int)(dist: Double): Double = dry(distMax)(1)(dist)

  def dry(distMax: Int)(flow: Int)(dist: Double): Double = {
    if (dist > distMax) 0 else MAX_MOISTURE * math.pow(-1.0 / distMax.toDouble * dist + 1, flow + 1)
  }
}
