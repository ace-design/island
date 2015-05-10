package eu.ace_design.island.game

import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import eu.ace_design.island.stdlib.Biomes._
import org.json.{JSONArray, JSONObject}
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ActionsTest extends SpecificationWithJUnit {

  "ActionsTest Specifications".title

  private def exec(actions: Seq[Result], g: Game): Game = {
    if (actions.isEmpty) g else exec(actions.tail, (g updatedBy actions.head)._1)
  }

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
    "reject a landing that leaves no one on the boad" in {
      val bad = Land(creek = "c", people = 50)
      bad.buildResult(b,g) must throwAn[IllegalArgumentException]
    }
    "reject a landing with 0- people" in {
      Land(creek = "c", people = 0).buildResult(b,g) must throwAn[IllegalArgumentException]
      Land(creek = "c", people = -1).buildResult(b,g) must throwAn[IllegalArgumentException]
    }
    "store the information about the landing" in {
      true must beTrue
    }
  }

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


  "The ActionParser" should {

    "reject irrelevant strings used as input" in {
      ActionParser("") must throwAn[IllegalArgumentException]
      ActionParser("{}") must throwAn[IllegalArgumentException]
      ActionParser("""{ "action": "pouet" }""") must throwAn[IllegalArgumentException]
    }

    "build a Stop action when asked to" in {
      val action = ActionParser(""" { "action": "stop" } """)
      action must beAnInstanceOf[Stop]
    }

    "build an Explore action when asked to" in {
      val action = ActionParser(""" { "action": "explore" } """)
      action must beAnInstanceOf[Explore]
    }

    "build a Land action when asked to" in {
      val action = ActionParser(""" { "action": "land", "parameters": { "creek": "xx", "people": 42 } } """)
      action must beAnInstanceOf[Land]
      val land = action.asInstanceOf[Land]
      land.creek must_== "xx"
      land.people must_== 42
    }

    "build a MoveTo action when asked to" in {
      val action = ActionParser(""" { "action": "move_to", "parameters": { "direction": "N" } } """)
      action must beAnInstanceOf[MoveTo]
      val moveTo = action.asInstanceOf[MoveTo]
      moveTo.direction must_== Directions.NORTH
    }

    "build a Scout action when asked to" in {
      val action = ActionParser(""" { "action": "scout", "parameters": { "direction": "S" } } """)
      action must beAnInstanceOf[Scout]
      val scout = action.asInstanceOf[Scout]
      scout.direction must_== Directions.SOUTH
    }

    "build an Exploit action when asked to" in {
      val action = ActionParser(""" { "action": "exploit", "parameters": { "resource": "WOOD" } } """)
      action must beAnInstanceOf[Exploit]
      val exploit = action.asInstanceOf[Exploit]
      exploit.resource must_== Resources.WOOD
      val illegal = """ { "action": "exploit", "parameters": { "resource": "FOO" } } """
      ActionParser(illegal) must throwAn[IllegalArgumentException]
    }

    "build a Glimpse action when asked for" in {
      val action = ActionParser(""" { "action": "glimpse", "parameters": { "range": 4, "direction": "S" } } """)
      action must beAnInstanceOf[Glimpse]
      val glimpse = action.asInstanceOf[Glimpse]
      glimpse.direction must_== Directions.SOUTH
      glimpse.range must_== 4
    }

    "build a Transform action when asked for" in {
      val action = ActionParser("""{ "action": "transform", "parameters": { "WOOD": 10, "QUARTZ": 100 } }""")
      action must beAnInstanceOf[Transform]
      val transform = action.asInstanceOf[Transform]
      transform.materials must haveSize(2)
      transform.materials must contain(Resources.WOOD -> 10)
      transform.materials must contain(Resources.QUARTZ -> 100)
    }

    "translate one-letter codes to directions" in {
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "N" } """)) must_== Directions.NORTH
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "S" } """)) must_== Directions.SOUTH
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "E" } """)) must_== Directions.EAST
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "W" } """)) must_== Directions.WEST
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "X" } """)) must throwAn[Exception]
    }


  }

}