package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, Point}
import eu.ace_design.island.map.{IsBorder, IsWater, IslandMap}

/**
 * This process identify the faces considered as "borders", i.e., touching the external boundaries of the map
 *
 * It annotates the faces with the IsBorder property
 */
object IdentifyBorders extends Process  {

  override def apply(m: IslandMap): IslandMap = {
    info("Annotating faces")
    // Extract the points located on the map border
    val isBorderValue: Double => Boolean = { d => d <= 0 || d >= m.mesh.size.get }
    val isBorderVertex: Point => Boolean = { p => isBorderValue(p.x) || isBorderValue(p.y)  }
    val borderVertices = m.mesh.vertices.queryReferences(isBorderVertex)

    // Identify the faces that involve such points
    val isBorder: Face => Boolean = { f => (f.vertices(m.mesh.edges) & borderVertices).nonEmpty }
    val borderFaces = m.mesh.faces.queryReferences(isBorder)
    debug("Faces tagged as border: " + borderFaces.toSeq.sorted.mkString("(",",",")") )

    // Update the properties for the identified faces and vertices
    val fProps = m.faceProps bulkAdd (borderFaces -> IsBorder()) bulkAdd (borderFaces -> IsWater())
    val vProps = m.vertexProps bulkAdd(borderVertices -> IsBorder())
    m.copy(faceProps = fProps, vertexProps = vProps)
  }

}
