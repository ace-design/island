package eu.ace_design.island.game

import eu.ace_design.island.bot.IExplorerRaid
import org.specs2.mutable._
import org.specs2.mock.Mockito
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineTest extends SpecificationWithJUnit with Mockito {

  "EngineTest Specifications".title


  "for the sake of error handling, the engine" should {
    val emptyBoard = mock[GameBoard]; val emptyGame = mock[Game]

    "detect exception thrown while initializing" in {
      // mocking the environment
      val explorer = mock[IExplorerRaid]
      explorer.initialize(anyString) throws new RuntimeException("error in explorer init")
      // starting the engine
      val engine = new Engine(emptyBoard, emptyGame)
      val events = engine.run(explorer)
      events.size must_== 2 // initialization context + exception event
      there was one(explorer).initialize(anyString)
      there was no(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
    }
    "detect actions that are not legal json"  in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": geek """
      val engine = new Engine(emptyBoard, emptyGame)
      val events = engine.run(explorer)
      events.size must_== 2 // initialization context + exception event
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
    }
    "detect unknown actions" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "not_a_real_action" }"""
      val engine = new Engine(emptyBoard, emptyGame)
      val events = engine.run(explorer)
      events.size must_== 3 // initialization context + received action + exception event
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
    }
    "stop when no more budget is available" in {
     true must beTrue
    }

  }

  "under normal conditions, the engine" should {
    "stop"
  }


}
