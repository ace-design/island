package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import eu.ace_design.island.stdlib.Biomes._
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GlimpseTest extends SpecificationWithJUnit {

  "Glimpse Action Specifications".title

  val g = Game(Budget(800), Crew(50), Set((Resources.WOOD, 600)))
  val b = GameBoard(size = 600, m = null, pois = Map((0,0) -> Set(Creek(identifier = "c", location = None))))

  "The Glimpse action" should {
    val t00 = Tile(biomes = Set((TROPICAL_RAIN_FOREST, 30.3), (MANGROVE, 69.7)))
    val t01 = Tile(biomes = Set((TUNDRA, 20.0), (TROPICAL_RAIN_FOREST, 50.3), (MANGROVE, 39.7)))
    val t02 = Tile(biomes = Set((TUNDRA, 30.3), (LAKE, 59.7), (GLACIER, 10.0)))
    val t03 = Tile(biomes = Set((GLACIER, 30.3), (ALPINE, 59.7), (LAKE, 10.0)))
    val board = GameBoard(size = 600, m = null,
      tiles = Map( (0,0) -> t00, (0,1) -> t01, (0,2) -> t02, (0,3) -> t03))
    "reject a negative or null range" in {
      Glimpse(range = -1, direction = Directions.NORTH) must throwAn[IllegalArgumentException]
      Glimpse(range = 0, direction = Directions.NORTH) must throwAn[IllegalArgumentException]
    }
    "reject a range greater than 4" in {
      Glimpse(range = 5, direction = Directions.NORTH) must throwAn[IllegalArgumentException]
    }
    "support exploration at different level of details" in {
      val g1 = Glimpse(1, Directions.SOUTH)
      val r1 = g1.buildResult(board, exec(Seq(MovedBoatResult(loc = (0,0), men = 2)), g)).asInstanceOf[GlimpseResult]
      r1.asked must_== 1 ; r1.report must haveSize(1); r1.report(0).length() must_== 2

      val g2 = Glimpse(2, Directions.SOUTH)
      val r2 = g2.buildResult(board, exec(Seq(MovedBoatResult(loc = (0,0), men = 2)), g)).asInstanceOf[GlimpseResult]
      r2.asked must_== 2;  r2.report must haveSize(2)
      r2.report(0).length() must_== 2; r2.report(1).length() must_== 3

      val g3 = Glimpse(3, Directions.SOUTH)
      val r3 = g3.buildResult(board, exec(Seq(MovedBoatResult(loc = (0,0), men = 2)), g)).asInstanceOf[GlimpseResult]
      r3.asked must_== 3;  r3.report must haveSize(3)
      r3.report(0).length() must_== 2; r3.report(1).length() must_== 3; r3.report(2).length must_== 3

      val g4 = Glimpse(4, Directions.SOUTH)
      val r4 = g4.buildResult(board, exec(Seq(MovedBoatResult(loc = (0,0), men = 2)), g)).asInstanceOf[GlimpseResult]
      r4.asked must_== 4;  r4.report must haveSize(4)  ;
      r4.report(0).length() must_== 2; r4.report(1).length() must_== 3; r4.report(2).length must_== 3
      r4.report(3).length() must_== 1
    }
    "remove empty tiles when reaching the border of the world" in {
      val g2 = Glimpse(2, Directions.NORTH)
      val r2 = g2.buildResult(board, exec(Seq(MovedBoatResult(loc = (0,0), men = 2)), g)).asInstanceOf[GlimpseResult]
      r2.report must haveSize(1)
    }
  }


}