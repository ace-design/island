package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FlyTest extends SpecificationWithJUnit with Mockito {

  "Fly Action Specifications".title

  val board = mock[GameBoard]
  board.size returns 600
  board.pois returns  Map((0,0) -> Set(Creek(identifier = "c", location = None)))
  board.tiles returns (for (x <- 0 until 60; y <- 0 until 60) yield (x,y) -> Tile()).toMap

  val g = Game(Budget(800), Crew(50), Set((Resources.WOOD, 600)))

  val plane = Plane(10,10,Directions.EAST)
  val gPlane = g.copy(plane = Some(plane))

  "The fly action" should {

    val action = Fly()

    "not be available with no plane on the board" in {
      action.buildResult(board,g) must throwAn[IllegalArgumentException]
    }

    "move the plane to another location" in {
      val res = action.buildResult(board,gPlane)
      res must beAnInstanceOf[MovedPlaneResult]
      gPlane.plane.get.position must_!= res.asInstanceOf[MovedPlaneResult].planeLoc
    }

  }


}