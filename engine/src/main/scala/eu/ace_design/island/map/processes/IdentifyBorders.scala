package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.{Face, Point}
import eu.ace_design.island.map.{IsBorder, IsWater, IslandMap}

/**
 * This process identify the faces considered as "borders", i.e., touching the external boundaries of the map. As we
 * are building an island, border faces are also considered as Water faces.
 *
 * Pre-conditions:
 *   - None
 *
 * Post-conditions:
 *   - Border faces that touches the edge of the map are annotated as IsBorder(true) and IsWater(true).
 *     Others are not impacted
 *   - Vertices touching the edge of the map are annotated as IsBorder(true), and IsWater(true).
 *     Others are not impacted.
 */
object IdentifyBorders extends Process  {

  override def apply(m: IslandMap): IslandMap = {
    info("Annotating faces")
    // Extract the points located on the map border
    val isBorderValue: Double => Boolean = { d => d <= 0 || d >= m.size }
    val isBorderVertex: Point => Boolean = { p => isBorderValue(p.x) || isBorderValue(p.y)  }
    val borderVertices = m.findVertexRefsWith(isBorderVertex)

    // Identify the faces that involve such points
    val isBorder: Face => Boolean = { f => (m.cornerRefs(f) & borderVertices).nonEmpty }
    val borderFaces = m.findFaceRefsWith(isBorder)
    debug("Faces tagged as border: " + borderFaces.toSeq.sorted.mkString("(",",",")") )

    // Update the properties for the identified faces and vertices
    val fProps = m.faceProps bulkAdd (borderFaces -> IsBorder()) bulkAdd (borderFaces -> IsWater())
    val vProps = m.vertexProps bulkAdd(borderVertices -> IsBorder()) bulkAdd (borderVertices -> IsWater())
    m.copy(faceProps = fProps, vertexProps = vProps)
  }

}
