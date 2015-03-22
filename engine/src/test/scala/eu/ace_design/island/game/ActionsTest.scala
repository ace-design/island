package eu.ace_design.island.game

import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import org.json.JSONObject
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
      Land(creek = "c", people = 0).buildResult(b,g) must throwAn[IllegalArgumentException]
    }
    "store the information about the landing" in {

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

    "translate one-letter codes to directions" in {
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "N" } """)) must_== Directions.NORTH
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "S" } """)) must_== Directions.SOUTH
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "E" } """)) must_== Directions.EAST
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "W" } """)) must_== Directions.WEST
      ActionParser.letter2Direction(new JSONObject(""" { "direction": "X" } """)) must throwAn[Exception]
    }


  }

}