package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, Registry}
import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}

/**
 * A face is considered as an ocean one if it is a water face connected to the borders of the map. Lakes are faces
 * identified as water but not as ocean.
 */
object IdentifyLakesAndOcean extends Process {

  import ExistingWaterKind.{OCEAN, LAKE}

  override def apply(m: IslandMap): IslandMap = {
    info("Annotating faces")
    val borders = getRefs(m, IsBorder())
    val oceans = propagate(borders, m.faceProps, m.mesh.faces, IsWater())
    val water = getRefs(m, IsWater())
    val lakes = water diff oceans
    debug("Faces tagged as ocean: " + oceans.toSeq.sorted.mkString("(",",",")"))
    debug("Faces tagged as lake: "  + lakes.toSeq.sorted.mkString("(",",",")"))
    val fProps = m.faceProps bulkAdd (oceans -> WaterKind(OCEAN)) bulkAdd (lakes -> WaterKind(LAKE))
    m.copy(faceProps = fProps)
  }

  /**
   * Returns the set of references to the faces holding a given property
   * @param m the map containing the faces
   * @param prop the property one is looking for
   * @return the set of integer references for such faces
   */
  private def getRefs(m: IslandMap, prop: Property[_]): Set[Int] = {
    val finder = m.faceProps.project(m.mesh.faces) _
    finder(Set(prop)) map { f => m.mesh.faces(f).get }
  }

  /**
   * Compute the transitive closure of a given property, based on an initial set of faces. If a face satisfy a given
   * property, its neighbors are then transitively subject to consideration
   * @param init the initial set of faces to investigate
   * @param props the PropertySet associated to the faces
   * @param faces the FaceRegistry (used to retrieve neighbors)
   * @param p the property to propagate transitively.
   * @return a set of references representing the transitive closure of 'p', starting with init.
   */
  private def propagate(init: Set[Int], props: PropertySet, faces: Registry[Face], p: Property[_]): Set[Int] = {
    // internal function used to compute the transitive closure in an accumulator
    def loop(candidates: Set[Int], acc: Set[Int]): Set[Int] = candidates.headOption match {
      case None => acc                             // no more candidate, the accumulator is the result
      case Some(c) => props.check(c, p) match {
        case false  => loop(candidates.tail, acc)  // does not satisfy 'p' => forgot about it
        case true   => {
          val newAcc = acc + c                              // satisfy 'p' => must be included in the accumulator
          val subjects = faces(c).neighbors.get diff newAcc // investigating neighbors not already in the accumulator
          loop(candidates.tail ++ subjects, newAcc)         // continue the computation on these new subjects
        }
      }
    }
    loop(init, Set())
  }
}
