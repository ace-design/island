package eu.ace_design.island.map.processes

import eu.ace_design.island.map.{HasForPitch, IslandMap}

class AssignPitchTest extends ProcessTestTrait {

  "AssignPitchTest Specifications".title

  override val preconditions: IslandMap => IslandMap = nothing
  override val processUnderTest = AssignPitch

  val pitches = result.faceProps.restrictedTo(HasForPitch())

  "The AssignPitch process" should {

    "assign a positive pitch  to each face" in {
      result.faceRefs foreach { pitches(_) must beGreaterThanOrEqualTo(0.0) }
      result.faceRefs.size must_== pitches.size
    }

  }

}