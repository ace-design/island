package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{IsWater, DiskShape}
import org.specs2.mutable._

class IdentifyWaterAreaTest extends SpecificationWithJUnit {

  "IdentifyWaterAreaTest Specifications".title

  "The IdentifyWaterArea process" should {
    val process = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8), threshold = 30)
    val updated = process(entry)   // no pre-conditions

    "annotate all the faces with IsWater properties" in {
      val waters = updated.findFacesWith(Set(IsWater()))
      val lands = updated.findFacesWith(Set(!IsWater()))
      waters ++ lands must_== mesh.faces.values
    }
  }

  "The AlignVertexWaterBasedOnFaces process" should {
    val precondition = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8), threshold = 30)
    val updated = AlignVertexWaterBasedOnFaces(precondition(entry))

    "consider vertices involved in land faces as land" in {
      val vertices = updated.findFacesWith(Set(!IsWater())) flatMap { f => updated.cornerRefs(f) }
      vertices map { updated.vertexProps.check(_,!IsWater()) } must contain(beTrue)
    }

    "align faces' center to their associated face" in {
      val faces = updated.findFacesWith(Set(!IsWater()))
      faces foreach { f =>
        val ref = updated.faceRef(f)
        val faceVal = updated.faceProps.getValue(ref,IsWater())
        updated.vertexProps.check(f.center, IsWater(faceVal)) must beTrue
      }
      true must beTrue
    }

    "tag all vertices defined in the map" in {
      val vertices = updated.vertices
      val landVertices  = updated.findVerticesWith(Set(!IsWater()))
      val waterVertices = updated.findVerticesWith(Set(IsWater()))
      (landVertices ++ waterVertices) must_== vertices
    }

  }

}