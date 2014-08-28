package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.Point
import eu.ace_design.island.map.{IslandMap, IsWater, IsBorder}

class IdentifyBordersTest extends ProcessTestTrait {

  "IdentifyBordersTest Specifications".title

  override val preconditions: IslandMap => IslandMap = m => m
  override val updated = IdentifyBorders(preconditions(entry))  // no pre-conditions

  "The IdentifyBorders process" should {

    val borderPoints = updated.findVerticesWith(Set(IsBorder()))
    val borderFaces  = updated.findFacesWith(Set(IsBorder()))

    "annotate with IsBorder the faces that touch the external boundary" in {
      // This trick only work because we are using a grid-based generator
      updated.faceProps.size must_== 4 * (math.sqrt(FACES).toInt - 1)
    }
    "annotate as border the points that touch the edge of the map" in {
      val check = (p: Point) => p.x == 0 || p.x == updated.size || p.y == 0 || p.y == updated.size
      borderPoints foreach { check(_) must beTrue }
      updated.vertexProps.size must beGreaterThan(0)
    }
    "leave faces that are not border one unchanged" in {
      val refs = borderFaces map { f => updated.faceRef(f) }
      val otherFaces = updated.faceRefs diff refs
      otherFaces foreach { ref => updated.faceProps.get(ref) must_== entry.faceProps.get(ref) }
      true must beTrue // to return a spec fragment and thus allow compilation.
    }
    "leave vertices that are not borders unchanged" in {
      val refs = borderPoints map { p => updated.vertexRef(p) }
      val otherPoints = updated.vertexRefs diff refs
      otherPoints foreach { ref => updated.vertexProps.get(ref) must_== entry.vertexProps.get(ref) }
      true must beTrue // to return a spec fragment and thus allow compilation.
    }
    "automatically consider border elements (faces and vertices) as water ones" in {
      val faceRefs = borderFaces map { f => updated.faceRef(f) }
      val verticesRefs = borderPoints map { p => updated.vertexRef(p) }
      faceRefs foreach { updated.faceProps.check(_, IsWater()) must beTrue }
      verticesRefs foreach { updated.vertexProps.check(_, IsWater()) must beTrue }
      true must beTrue
    }
  }

}