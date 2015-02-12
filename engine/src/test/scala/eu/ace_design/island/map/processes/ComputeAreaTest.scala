package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{HasForArea, IslandMap}
import eu.ace_design.island.map.resources.PIXEL_FACTOR

class ComputeAreaTest extends ProcessTestTrait {

  "ComputeAreaTest Specifications".title

  override val preconditions: IslandMap => IslandMap = nothing
  override val processUnderTest = ComputeArea

  val areas = result.faceProps.restrictedTo(HasForArea())

  "The ComputeArea process" should {

    "assign a positive area  to each face" in {
      result.faceRefs foreach { areas(_) must beGreaterThan(0.0) }
      result.faceRefs.size must_== areas.size
    }

    "be precise enough to return the area of the map (in squared meters) while adding all the computed areas" in {
      val total = (0.0 /: areas.values) { _ + _ }
      total must_== (SIZE * SIZE) * (PIXEL_FACTOR * PIXEL_FACTOR)
    }

  }


}