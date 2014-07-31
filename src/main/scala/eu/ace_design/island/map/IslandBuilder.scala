package eu.ace_design.island.map

import eu.ace_design.island.geom._
import eu.ace_design.island.util.Log

/**
 * An IslandBuilder is a sequence of Process used to build an Island map
 */
trait IslandBuilder {

  // The size of the map
  def size: Int

  /**
   * Steps is the sequence (order matters) of processes used to create the map
   */
  protected val steps: Seq[Process]

  /**
   * Exploit a given mesh to build a map on top of it
   * @param m the mesh to be used as entry point
   * @return an island map, based on the sequential execution of the different steps of this builder
   */
  def apply(m: Mesh): IslandMap = {
    require(m.size.isDefined, "Cannot build an island without knowing its size")
    require(m.size.get == size, "Size of mesh and map must be consistent")
    (IslandMap(m) /: steps) { (map, p) => p(map) }
  }
}


/**
 * A process works on a map to produce an updated map, based on its semantics. To be used as a Step in a builder.
 *
 * Remark: This trait is sealed, and cannot be implemented outside of this file.
 */
sealed trait Process {

  /**
   * This function literally apply this steps on a given map, producing a new one that contains the properties
   * introduced by this process
   * @param m the input map
   * @return the enhanced map
   */
  def apply(m: IslandMap): IslandMap

  // TODO introduce a mechanism to expose properties requirements (e.g., this process assume p to be present)
}


/**
 * This process identify the faces considered as "borders", i.e., touching the external boundaries of the map
 *
 * It annotates the faces with the IsBorder property
 */
object IdentifyBorders extends Process with Log {

  override def apply(m: IslandMap): IslandMap = {
    logger.info("Borders computation: started")
    // Extract the points located on the map border
    val isBorderValue: Double => Boolean = { d => d <= 0 || d >= m.mesh.size.get }
    val isBorderVertex: Point => Boolean = { p => isBorderValue(p.x) || isBorderValue(p.y)  }
    val borderVertices = m.mesh.vertices.queryReferences(isBorderVertex)

    // Identify the faces that involve such points
    val isBorder: Face => Boolean = { f => (f.vertices(m.mesh.edges) & borderVertices).nonEmpty }
    val borderFaces = m.mesh.faces.queryReferences(isBorder)

    // Update the properties for the identified faces
    val result = m.copy(faceProps = m.faceProps bulkAdd (borderFaces -> IsBorder()) )
    logger.info("Borders computation: ended")
    result
  }

}

/**
 * A face is annotated as Water if it is one of the border of map, or if it involves a number of vertices located in
 * a water area (according to a given IslandShape) greater than a given threshold
 *
 * @param shape the IslandShape used for this Island
 * @param threshold the threshold (in [0,100]) to decide if a face is a water one
 */
case class IdentifyWaterArea(shape: IslandShape, threshold: Int) extends Process with Log {
  require(threshold >= 0,   "threshold must be in [0,100]")
  require(threshold <= 100, "threshold must be in [0,100]")

  override def apply(m: IslandMap): IslandMap = {
    logger.info("Annotating vertices")
    val isWaterVertex = shape.isWater _
    val pRefs = m.mesh.vertices.queryReferences(isWaterVertex) // Find all the vertices matching the given shape
    val vProps = m.vertexProps bulkAdd (pRefs -> IsWater())
    logger.info("done")

    logger.info("Annotating faces")
    val isWaterFace: Face => Boolean = { f =>
      val ref = m.mesh.faces(f).get
      val isBorder = m.faceProps.check(ref, IsBorder())
      val vertices = f.vertices(m.mesh.edges)
      val waterVertices = vertices filter { pRef => vProps.check(pRef, IsWater()) }
      val isGreaterThanThreshold = (waterVertices.size.toFloat / vertices.size) * 100 > threshold
      isBorder || isGreaterThanThreshold
    }
    val waterFaceRefs = m.mesh.faces.queryReferences(isWaterFace)
    val landFaceRefs = m.mesh.faces.references diff waterFaceRefs
    val fProps = m.faceProps bulkAdd (waterFaceRefs -> IsWater()) bulkAdd (landFaceRefs -> !IsWater())
    logger.info("done")
    m.copy(vertexProps = vProps, faceProps = fProps)
  }
}

/**
 * A face is considered as an ocean one if it is a water face connected to the borders of the map. Lakes are faces
 * identified as water but not as ocean.
 */
object IdentifyLakesAndOcean extends Process with Log {
  import ExistingWaterKind.{OCEAN, LAKE}

  override def apply(m: IslandMap): IslandMap = {
    logger.info("Annotating faces")
    val borders = getRefs(m, IsBorder())
    val oceans = propagate(borders, m.faceProps, m.mesh.faces, IsWater())
    val water = getRefs(m, IsWater())
    val lakes = water diff oceans
    val fProps = m.faceProps bulkAdd (oceans -> WaterKind(OCEAN)) bulkAdd (lakes -> WaterKind(LAKE))
    logger.info("done")
    m.copy(faceProps = fProps)
  }

  /**
   * Returns the ste of references to the faces holding a given property
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