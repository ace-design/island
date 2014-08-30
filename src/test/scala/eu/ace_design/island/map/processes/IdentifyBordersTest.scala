package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.Point
import eu.ace_design.island.map.{IslandMap, IsWater, IsBorder}

class IdentifyBordersTest extends ProcessTestTrait {

  "IdentifyBordersTest Specifications".title

  override val preconditions: IslandMap => IslandMap = m => m
  override val result = IdentifyBorders(preconditions(entry))  // no pre-conditions

  "The IdentifyBorders process" should {

    val borderPoints = result.findVerticesWith(Set(IsBorder()))
    val borderFaces  = result.findFacesWith(Set(IsBorder()))

    "annotate with IsBorder the faces that touch the external boundary" in {
      // This trick only work because we are using a grid-based generator
      result.faceProps.size must_== 4 * (math.sqrt(FACES).toInt - 1)
    }
    "annotate as border the points that touch the edge of the map" in {
      val check = (p: Point) => p.x == 0 || p.x == result.size || p.y == 0 || p.y == result.size
      borderPoints foreach { check(_) must beTrue }
      result.vertexProps.size must beGreaterThan(0)
    }
    "leave faces that are not border one unchanged" in {
      val refs = borderFaces map { f => result.faceRef(f) }
      val otherFaces = result.faceRefs diff refs
      otherFaces foreach { ref => result.faceProps.get(ref) must_== entry.faceProps.get(ref) }
      true must beTrue // to return a spec fragment and thus allow compilation.
    }
    "leave vertices that are not borders unchanged" in {
      val refs = borderPoints map { p => result.vertexRef(p) }
      val otherPoints = result.vertexRefs diff refs
      otherPoints foreach { ref => result.vertexProps.get(ref) must_== entry.vertexProps.get(ref) }
      true must beTrue // to return a spec fragment and thus allow compilation.
    }
    "automatically consider border elements (faces and vertices) as water ones" in {
      val faceRefs = borderFaces map { f => result.faceRef(f) }
      val verticesRefs = borderPoints map { p => result.vertexRef(p) }
      faceRefs foreach { result.faceProps.check(_, IsWater()) must beTrue }
      verticesRefs foreach { result.vertexProps.check(_, IsWater()) must beTrue }
      true must beTrue
    }
  }

}