package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, Point}
import eu.ace_design.island.map.{IsBorder, IsWater, IslandMap}
import eu.ace_design.island.util.{LogSilos, Logger}

/**
 * This process identify the faces considered as "borders", i.e., touching the external boundaries of the map
 *
 * It annotates the faces with the IsBorder property
 */
object IdentifyBorders extends Process with Logger {
  val silo = LogSilos.MAP_GEN

  override def apply(m: IslandMap): IslandMap = {
    info("IdentifyBorders / Annotating faces")
    // Extract the points located on the map border
    val isBorderValue: Double => Boolean = { d => d <= 0 || d >= m.mesh.size.get }
    val isBorderVertex: Point => Boolean = { p => isBorderValue(p.x) || isBorderValue(p.y)  }
    val borderVertices = m.mesh.vertices.queryReferences(isBorderVertex)

    // Identify the faces that involve such points
    val isBorder: Face => Boolean = { f => (f.vertices(m.mesh.edges) & borderVertices).nonEmpty }
    val borderFaces = m.mesh.faces.queryReferences(isBorder)
    debug("Faces tagged as border: " + borderFaces.toSeq.sorted.mkString("(",",",")") )

    // Update the properties for the identified faces
    m.copy(faceProps = m.faceProps bulkAdd (borderFaces -> IsBorder()) bulkAdd (borderFaces -> IsWater()))
  }

}
