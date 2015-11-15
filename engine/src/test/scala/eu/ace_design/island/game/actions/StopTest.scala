package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StopTest extends SpecificationWithJUnit {

  "Stop Action Specifications".title

  val g = Game(Budget(800), Crew(50), Set((Resources.WOOD, 600)))
  val b = GameBoard(size = 600, m = null, pois = Map((0,0) -> Set(Creek(identifier = "c", location = None))))


  "The Stop action" should {
    val action = Stop()

    "cost nothing if not landed" in {
      action.buildResult(b,g).shouldStop must beTrue
      action.computeCost(b,g) must_== 0.0
    }
    "cost something if landed" in {
      val game = exec(Seq(MovedBoatResult(loc = (10,10), men = 30)), g)
      action.computeCost(b,game) must beGreaterThan(0.0)
    }
  }

}