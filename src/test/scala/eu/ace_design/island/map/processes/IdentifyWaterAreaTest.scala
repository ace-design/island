package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{IslandMap, IsWater, DiskShape}
import org.specs2.mutable._

class IdentifyWaterAreaTest extends ProcessTestTrait {

  "IdentifyWaterAreaTest Specifications".title

  val process = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8), threshold = 30)

  override val preconditions: IslandMap => IslandMap = m => m
  override val result: IslandMap = process(entry)

  "The IdentifyWaterArea process" should {

    "annotate all the faces with IsWater properties" in {
      val waters = result.findFacesWith(Set(IsWater()))
      val lands = result.findFacesWith(Set(!IsWater()))
      waters ++ lands must_== mesh.faces.values
    }
  }
}

class AlignVertexWaterBasedOnFacesTest extends ProcessTestTrait {

  "AlignVertexWaterBasedOnFacesTest Specifications".title

  val disk = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8)
  override val preconditions: IslandMap => IslandMap = m => IdentifyWaterArea(shape = disk, threshold = 30)(m)
  override val result = AlignVertexWaterBasedOnFaces(preconditions(entry))

  "The AlignVertexWaterBasedOnFaces process" should {


    "consider vertices involved in land faces as land" in {
      val vertices = result.findFacesWith(Set(!IsWater())) flatMap { f => result.cornerRefs(f) }
      vertices map { result.vertexProps.check(_,!IsWater()) } must contain(beTrue)
    }

    "align faces' center to their associated face" in {
      val faces = result.findFacesWith(Set(!IsWater()))
      faces foreach { f =>
        val ref = result.faceRef(f)
        val faceVal = result.faceProps.getValue(ref,IsWater())
        result.vertexProps.check(f.center, IsWater(faceVal)) must beTrue
      }
      true must beTrue
    }

    "tag all vertices defined in the map" in {
      val vertices = result.vertices
      val landVertices  = result.findVerticesWith(Set(!IsWater()))
      val waterVertices = result.findVerticesWith(Set(IsWater()))
      (landVertices ++ waterVertices) must_== vertices
    }

  }

}