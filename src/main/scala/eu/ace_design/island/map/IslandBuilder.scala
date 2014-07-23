package eu.ace_design.island.map

import eu.ace_design.island.geom.{Mesh, Face, Edge, Point}

/**
 * An IslandBuilder is a sequence of Process used to build an Island map
 */
trait IslandBuilder {

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
object IdentifyBorders extends Process {

  override def apply(m: IslandMap): IslandMap = {
    // Extract the points located on the map border
    val isBorderValue: Double => Boolean = { d => d <= 0 || d >= m.mesh.size.get }
    val isBorderVertex: Point => Boolean = { p => isBorderValue(p.x) || isBorderValue(p.y)  }
    val borderVertices = m.mesh.vertices.queryReferences(isBorderVertex)

    // Identify the faces that involve such points
    val isBorder: Face => Boolean = { f => (f.vertices(m.mesh.edges) & borderVertices).nonEmpty }
    val borderFaces = m.mesh.faces.queryReferences(isBorder)

    // Update the properties for the identified faces
    m.copy(faceProps = m.faceProps bulkAdd (borderFaces -> IsBorder()) )
  }

}