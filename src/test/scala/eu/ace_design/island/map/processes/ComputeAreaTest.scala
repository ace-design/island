package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{HasForArea, IslandMap}

class ComputeAreaTest extends ProcessTestTrait {

  "ComputeAreaTest Specifications".title

  override val preconditions: IslandMap => IslandMap = m => m
  override val result = ComputeArea(preconditions(entry))

  val areas = result.faceProps.restrictedTo(HasForArea())

  "The ComputeArea process" should {

    "assign a positive area  to each face" in {
      result.faceRefs foreach { areas(_) must beGreaterThan(0.0) }
      result.faceRefs.size must_== areas.size
    }

    "be precise enough to return the area of the map (in squared meters) while adding all the computed areas" in {
      val total = (0.0 /: areas.values) { _ + _ }
      total must_== (SIZE * SIZE) * (ComputeArea.PIXEL_FACTOR * ComputeArea.PIXEL_FACTOR)
    }

  }


}