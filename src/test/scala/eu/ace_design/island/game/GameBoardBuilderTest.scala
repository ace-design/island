package eu.ace_design.island.game

import eu.ace_design.island.geom.{Point, MeshBuilderTestDataSet}
import eu.ace_design.island.map.IslandMap
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameBoardBuilderTest extends SpecificationWithJUnit {

  "GameBoardBuilderTest Specifications".title

  "The GameBoardBuilder" should {

    val m = IslandMap(MeshBuilderTestDataSet.mesh) // m.size == 200
    val builder = new GameBoardBuilder()   // chunk = DEFAULT_TILE_UNIT = 10
    val triangle = Set(Point(5.0, 6.0), Point(18.9, 28.3), Point(26.4, 15.5))

    "reject a chunk size incompatible with the size of the map" in {
      val erroneous = new GameBoardBuilder(chunk = 11)
      erroneous(m) must throwAn[IllegalArgumentException]  // 200 % 11 <> 0
    }

    "ensure that the size of the game board is the size of the used map" in {
      val board = builder(m)
      board must beAnInstanceOf[GameBoard]
      board.size must_== m.size
    }

    "identify in which tile is located a given point" in {
      builder.locate(Point(0.0,0.0))     must_== (0,0)
      builder.locate(Point(99.0, 188.0)) must_== (9, 18)
    }

    "identify the bounding box of a given face" in {
      // triangle(0) \in (0,0), triangle(1) \in (1,2) and triangle(2) \in (2,1)
      builder.boundingBox(triangle) must_== Set( (0,0), (0,1), (0,2),
                                                 (1,0), (1,1), (1,2),
                                                 (2,0), (2,1), (2,2) )
    }

    "identify the tiles covered by a given face" in {
      val coverage = builder.coverage(triangle)
      coverage.values.sum must beCloseTo(100.0, 0.0001) // The coverage algorithm is "almost" exact
      // The triangle is not located on the upper right (0,2) and lower left (2,0) of its bounding box
      coverage.keys.toSet must_== Set((0,0), (0,1), (1,0), (1,1), (1,2), (2,1), (2,2))
    }

  }

}