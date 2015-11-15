package eu.ace_design.island.game.actions

import eu.ace_design.island.game.{GameBoard, Crew, Budget, Game}
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LandTest extends SpecificationWithJUnit {

  "LandTest Specifications".title

  val g = Game(Budget(800), Crew(50), Set((Resources.WOOD, 600)))
  val b = GameBoard(size = 600, m = null, pois = Map((0,0) -> Set(Creek(identifier = "c", location = None))))

  "The Land action" should {
    val action = Land(creek = "c", people = 15)

    "be available even if not landed" in {
      action.buildResult(b,g) must not(throwAn[IllegalArgumentException])
      action.computeCost(b,g) must beGreaterThanOrEqualTo(0.0)
    }
    "reject landing on an unknown creek" in {
      val bad = Land(creek = "unkown", people = 15)
      bad.buildResult(b,g) must throwAn[IllegalArgumentException]
    }
    "reject a landing that leaves no one on the boat" in {
      val bad = Land(creek = "c", people = 50)
      bad.buildResult(b,g) must throwAn[IllegalArgumentException]
    }
    "reject a landing with 0- people" in {
      Land(creek = "c", people = 0).buildResult(b,g) must throwAn[IllegalArgumentException]
      Land(creek = "c", people = -1).buildResult(b,g) must throwAn[IllegalArgumentException]
    }
    "store the information about the landing" in {
      true must beTrue  // TODO ugly stuff here !! FIXME
    }
  }


}