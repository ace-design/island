package eu.ace_design.island.game

import org.json.JSONObject
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ActionsTest extends SpecificationWithJUnit {

  "ActionsTest Specifications".title

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
      val action = ActionParser(""" { "action": "exploit", "parameters": { "resource": "S" } } """)
      action must beAnInstanceOf[Scout]
      val scout = action.asInstanceOf[Scout]
      scout.direction must_== Directions.SOUTH
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