package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.Face
import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}

/**
 * A face is annotated as Water if it is one of the border of map, or if it involves a number of vertices located in
 * a water area (according to a given IslandShape) greater than a given threshold
 *
 * @param shape the IslandShape used for this Island
 * @param threshold the threshold (in [0,100]) to decide if a face is a water one
 */
case class IdentifyWaterArea(shape: IslandShape, threshold: Int) extends Process with Logger {
  require(threshold >= 0, "threshold must be in [0,100]")
  require(threshold <= 100, "threshold must be in [0,100]")

  val silo = LogSilos.MAP_GEN

  override def apply(m: IslandMap): IslandMap = {
    info("IdentifyWaterArea / Creating the shape")
    val isWaterVertex = shape.isWater _
    val pRefs = m.mesh.vertices.queryReferences(isWaterVertex) // Find all the vertices matching the given shape

    info("IdentifyWaterArea / Annotating faces")
    val isWaterFace: Face => Boolean = { f =>
      val ref = m.mesh.faces(f).get
      val isBorder = m.faceProps.check(ref, IsBorder())
      val vertices = f.vertices(m.mesh.edges)
      val waterVertices = vertices filter { r => pRefs.contains(r)}
      val isGreaterThanThreshold = (waterVertices.size.toFloat / vertices.size) * 100 > threshold
      isBorder || isGreaterThanThreshold
    }
    val waterFaceRefs = m.mesh.faces.queryReferences(isWaterFace)
    val landFaceRefs = m.mesh.faces.references diff waterFaceRefs
    debug("Faces tagged as water: " + waterFaceRefs.toSeq.sorted.mkString("(", ",", ")"))
    debug("Faces tagged as land: " + landFaceRefs.toSeq.sorted.mkString("(", ",", ")"))
    val fProps = m.faceProps bulkAdd (waterFaceRefs -> IsWater()) bulkAdd (landFaceRefs -> !IsWater())

    info("IdentifyWaterArea / Annotating vertices based on faces")
    // Interesting vertices are all the vertices not used as the center of a given face.
    val exceptCenters = m.mesh.vertices.references diff (m.mesh.faces.values map {
      _.center
    })
    // Land vertices are the one involved in a land faces. All the other are water
    val landVertices = (landFaceRefs map {
      m.mesh.faces(_).vertices(m.mesh.edges)
    }).flatten
    val waterVertices = exceptCenters diff landVertices
    // Others are water (excepting the centers of the faces)
    val vProps = m.vertexProps bulkAdd (waterVertices -> IsWater()) bulkAdd (landVertices -> !IsWater())

    m.copy(vertexProps = vProps, faceProps = fProps)
  }
}

