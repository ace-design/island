package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, Registry}
import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}

/**
 * A face is considered as an ocean one if it is a water face connected to the borders of the m. Lakes are faces
 * identified as water but not as ocean.
 *
 * Pre-conditions:
 *   - Faces touching the edge of the m are identified as "IsBorder(true)"
 *   - Faces are identified as "IsWater(b)", with b in {true, false}.
 *
 * Post-conditions:
 *   - Water faces connected to the border by a path are annotated as "WaterKind(OCEAN)"
 *   - Water faces which are not ocean ones are annotated as "WaterKind(LAKE)"
 */
object IdentifyLakesAndOcean extends Process {

  import ExistingWaterKind.{OCEAN, LAKE}

  override def apply(m: IslandMap): IslandMap = {
    info("Annotating faces")
    val borders = getRefs(m, IsBorder())
    val oceans = propagate(borders, m.faceProps, m, IsWater())
    val water = getRefs(m, IsWater())
    val lakes = water diff oceans
    debug("Faces tagged as ocean: " + oceans.toSeq.sorted.mkString("(",",",")"))
    debug("Faces tagged as lake: "  + lakes.toSeq.sorted.mkString("(",",",")"))
    val fProps = m.faceProps bulkAdd (oceans -> WaterKind(OCEAN)) bulkAdd (lakes -> WaterKind(LAKE))
    m.copy(faceProps = fProps)
  }

  /**
   * Returns the set of references to the faces holding a given property
   * @param m the m containing the faces
   * @param prop the property one is looking for
   * @return the set of integer references for such faces
   */
  private def getRefs(m: IslandMap, prop: Property[_]): Set[Int] = m.findFacesWith(Set(prop)) map { f => m.faceRef(f) }

  /**
   * Compute the transitive closure of a given property, based on an initial set of faces. If a face satisfy a given
   * property, its neighbors are then transitively subject to consideration
   * @param init the initial set of faces to investigate
   * @param props the PropertySet associated to the faces
   * @param m the IslandMap we are propagating in (used to retrieve neighbors of a given face)
   * @param p the property to propagate transitively.
   * @return a set of references representing the transitive closure of 'p', starting with init.
   */
  private def propagate(init: Set[Int], props: PropertySet, m: IslandMap, p: Property[_]): Set[Int] = {
    // internal function used to compute the transitive closure in an accumulator
    def loop(candidates: Set[Int], acc: Set[Int]): Set[Int] = candidates.headOption match {
      case None => acc                             // no more candidate, the accumulator is the result
      case Some(c) => props.check(c, p) match {
        case false  => loop(candidates.tail, acc)  // does not satisfy 'p' => forgot about it
        case true   => {
          val newAcc = acc + c                              // satisfy 'p' => must be included in the accumulator
          val subjects = m.face(c).neighbors.get diff newAcc // investigating neighbors not already in the accumulator
          loop(candidates.tail ++ subjects, newAcc)         // continue the computation on these new subjects
        }
      }
    }
    loop(init, Set())
  }
}
