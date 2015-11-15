package eu.ace_design.island.game

import eu.ace_design.island.game.actions._
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import eu.ace_design.island.stdlib.Biomes._
import eu.ace_design.island.stdlib.Resources._
import org.json.JSONObject
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ActionParserTest extends SpecificationWithJUnit {

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

    "build a Fly action when asked for" in {
      val action = ActionParser("""{ "action": "fly" }""")
      action must beAnInstanceOf[Fly]
    }

    "build an Heading action when asked for" in {
      val action = ActionParser("""{"action": "heading", "parameters": {"direction": "N"}}""")
      action must beAnInstanceOf[Heading]
    }

    "build an Echo action when asked for" in {
      val action = ActionParser("""{"action": "echo", "parameters": {"direction": "N"}}""")
      action must beAnInstanceOf[Echo]
    }

    "build a Scan action when asked for" in {
      val action = ActionParser("""{"action": "scan" }""")
      action must beAnInstanceOf[Scan]
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