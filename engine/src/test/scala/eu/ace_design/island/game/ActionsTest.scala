package eu.ace_design.island.game

import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.stdlib.Resources
import org.json.JSONObject
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mock.Mockito

@RunWith(classOf[JUnitRunner])
class ActionsTest extends SpecificationWithJUnit with Mockito {

  "ActionsTest Specifications".title

  val budget = Budget(800);  val crew = Crew(50); val objective = (Resources.WOOD, 600)
  val game   = Game(budget, crew, Set(objective))

  "The Stop action" should {
    val map   = mock[IslandMap] ; map.size   returns 600
    val board = mock[GameBoard] ; board.size returns 100 ; board.m returns map

    "be available even if not landed" in {
      val action = Stop()
      val (after, result) = action(board, game)
      result.cost must beGreaterThan(0)
      result.cost must beLessThanOrEqualTo(action.maxOverhead + action.maxOverhead/2)
      after.budget.remaining must_== game.budget.remaining - result.cost
    }

    "compute" in {
      val action = Stop()
      val (after, result) = action(board, game.moveBoat(loc = (10,10)))
      result.cost must beGreaterThan(17) // minimum cost with these data
      after.budget.remaining must_== game.budget.remaining - result.cost
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
      val action = ActionParser(""" { "action": "land", "parameters": { "deck": "xx", "people": 42 } } """)
      action must beAnInstanceOf[Land]
      val land = action.asInstanceOf[Land]
      land.deck must_== "xx"
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

    "translate one-letter codes to directions" in {
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "N" } """)) must_== Directions.NORTH
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "S" } """)) must_== Directions.SOUTH
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "E" } """)) must_== Directions.EAST
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "W" } """)) must_== Directions.WEST
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "X" } """)) must throwAn[Exception]
    }


  }


}