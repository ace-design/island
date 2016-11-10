package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import eu.ace_design.island.stdlib.PointOfInterests.{Creek, EmergencySite}
import eu.ace_design.island.stdlib.Resources
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ScanTest extends SpecificationWithJUnit with Mockito {

  "Scan Action Specifications".title

  val board = mock[GameBoard]
  board.size returns 600
  board.pois returns  Map((0,0) -> Set(Creek(identifier = "c", location = None)),
    (1,1) -> Set(EmergencySite(identifier = "s", location = None)))

  board.tiles returns (for (x <- 0 until 60; y <- 0 until 60) yield (x,y) -> Tile()).toMap

  val g = Game(Budget(800), Crew(50), Set((Resources.WOOD, 600)))

  val plane = Plane(1,1,Directions.EAST)
  val gPlane = g.copy(plane = Some(plane))

  "The scan action" should {

    val action = eu.ace_design.island.game.actions.Scan()

    "not be available with no plane on the board" in {
      action.buildResult(board,g) must throwAn[IllegalArgumentException]
    }

    "return pois" in {
      val res = action.buildResult(board, gPlane)
      res must beAnInstanceOf[ScanResult]
      val scan = res.asInstanceOf[ScanResult]
      scan.creeks must haveSize(1)
      scan.creeks.head must_== Creek(identifier = "c", location = None)
      scan.sites must haveSize(1)
      scan.sites.head must_== EmergencySite(identifier = "s", location = None)
    }

  }

}