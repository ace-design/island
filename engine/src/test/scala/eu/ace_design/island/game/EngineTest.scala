package eu.ace_design.island.game

import eu.ace_design.island.bot.IExplorerRaid
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Biomes._
import eu.ace_design.island.stdlib.Resources.WOOD
import org.specs2.mutable._
import org.specs2.mock.Mockito
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineTest extends SpecificationWithJUnit with Mockito {

  "EngineTest Specifications".title

  val emptyBoard = mock[GameBoard]
  emptyBoard.findPOIsByType(any) returns Set((10,10) -> Creek("c1", None), (0,0) -> Creek("border", None))
  emptyBoard.size returns 10
  emptyBoard.startingTile returns None
  val t0 = Tile(altitude = 3, biomes = Set((TUNDRA, 100.0)), stock = Set(Stock(WOOD, 30, 1.1)))
  val t1 = Tile(altitude = 12, biomes = Set((MANGROVE, 100.0)))
  emptyBoard.tiles returns Map((10,9) -> t1, (10,10) -> t0)
  emptyBoard.at(10,9)  returns t1 ; emptyBoard.at(10,10) returns t0
  emptyBoard.pois returns Map((10,10) -> Set(Creek("c1", None).asInstanceOf[PointOfInterest]), (0,0) -> Set(Creek("border", None)))
  emptyBoard.m returns mock[IslandMap]; emptyBoard.m.size returns 800

  val plane = Plane(1,1,Directions.EAST)
  val emptyGame = Game(Budget(600), Crew(50), Set()).copy(plane = Some(plane))

  "for the sake of error handling, the engine" should {

    "detect exception thrown while initializing" in {
      // mocking the environment
      val explorer = mock[IExplorerRaid]
      explorer.initialize(anyString) throws new RuntimeException("error in explorer init")
      // starting the engine
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      events.size must_== 2 // initialization context + exception event
      there was one(explorer).initialize(anyString)
      there was no(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
      g.isOK must beFalse
    }
    "detect actions that are not legal json"  in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": geek """
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      events.size must_== 2 // initialization context + exception event
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
      g.isOK must beFalse
    }
    "detect unknown actions" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "not_a_real_action" }"""
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      events.size must_== 3 // initialization context + received action + exception event
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
      g.isOK must beFalse
    }
    "exit with an error when no more budget is available" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "land",  "parameters": { "creek": "c1", "people": 30 } } }"""
      val engine = new Engine(emptyBoard, Game(Budget(1), Crew(50), Set()).copy(plane = Some(plane)))
      val (events, g) = engine.run(explorer)
      g.isOK must beFalse
      events.size must_== 3 // initialization context + received action + exception event
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was one(explorer).acknowledgeResults(anyString)
    }
  }

  "under normal conditions, the engine" should {

    val land  = """{ "action": "land", "parameters": { "creek": "c1", "people": 3 } } }"""
    val stop  = """{ "action": "stop" }"""
    val move  = """{ "action": "move_to", "parameters": { "direction": "N" } }"""
    val scout = """{ "action": "scout",   "parameters": { "direction": "N" } }"""
    val explore = """{ "action": "explore" }"""
    val exploit = """{"action": "exploit", "parameters": { "resource": "WOOD" } }"""


    "stop when asked for" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "stop" }"""
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beTrue
      events.size must_== 3 // initialization context + received action + end of game event
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was one(explorer).acknowledgeResults(anyString)
    }

    "support transition from aerial to terrestrial phase" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns land thenReturn stop
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beTrue
      g.plane must beNone
    }

    "reject landing with too much men" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "land", "parameters": { "creek": "c1", "people": 50 } } }"""
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beFalse
      events.size must_== 3 // initialization context + received action + end of game event
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
    }
    "reject landing with unknown creek" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "land", "parameters": { "creek": "cXX", "people": 3 } } }"""
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beFalse
      events.size must_== 3 // initialization context + received action + end of game event
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
    }
    "support the sequencing of operation, e.g., land then stop" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns land thenReturn stop
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beTrue
      there was one(explorer).initialize(anyString)
      there was two(explorer).takeDecision
      there was two(explorer).acknowledgeResults(anyString)
    }
    "reject moving without previous landing" in {
      val move = """{"action": "move_to", "parameters": { "direction": "E" } }"""
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns move
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beFalse
      there was one(explorer).initialize(anyString)
      there was one(explorer).takeDecision
      there was no(explorer).acknowledgeResults(anyString)
    }
    "reject moving out of the map" in {
      val borderLand = """{ "action": "land", "parameters": { "creek": "border", "people": 3 } } }"""
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns borderLand thenReturn  move
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beFalse
      there was one(explorer).initialize(anyString)
      there was two(explorer).takeDecision
      there was one(explorer).acknowledgeResults(anyString)
    }
    "support moving from one tile to another one" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns land thenReturn move thenReturn stop
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beTrue
      g.visited must_== Set((10,10), (10,9))
      g.budget.remaining must beLessThan(g.budget.initial)
    }
    "support the detection of unreachable tiles" in {
      val explorer = mock[IExplorerRaid]
      val borderLand = """{ "action": "land", "parameters": { "creek": "border", "people": 3 } } }"""
      explorer.takeDecision() returns borderLand thenReturn scout thenReturn stop
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beTrue
      g.budget.remaining must beLessThan(g.budget.initial)
    }
    "support scouting a tile" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns land thenReturn scout thenReturn stop
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beTrue
      g.budget.remaining must beLessThan(g.budget.initial)
    }
    "support exploring a tile" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns land thenReturn explore thenReturn stop
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beTrue
      g.budget.remaining must beLessThan(g.budget.initial)

    }
    "reject exploiting unavailable resource on a tile" in {
      val explorer = mock[IExplorerRaid]
      val exploit = """{"action": "exploit", "parameters": { "resource": "ORE" } }"""
      explorer.takeDecision() returns land thenReturn exploit thenReturn stop
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beFalse
      there was one(explorer).initialize(anyString)
      there was two(explorer).takeDecision
      there was one(explorer).acknowledgeResults(anyString)
    }
    "reject exploiting unavailable resource on a tile" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns land thenReturn exploit thenReturn stop
      val engine = new Engine(emptyBoard, emptyGame)
      val (events, g) = engine.run(explorer)
      g.isOK must beTrue
      g.budget.remaining must beLessThan(g.budget.initial)
    }

    "support flying from one zone to another one" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "fly" }""" thenReturn stop
      val plane = Plane(10,7,Directions.SOUTH)   // (10,10) is defined in the mock
      val engine = new Engine(emptyBoard, emptyGame.copy(plane = Some(plane)))
      val (_, g) = engine.run(explorer)
      g.isOK must beTrue
      g.plane.get.position must_!= plane.position
      g.budget.remaining must beLessThan(g.budget.initial)
    }

    "support heading changes" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "heading", parameters: { "direction": "W" } }""" thenReturn stop
      val plane = Plane(13,7,Directions.SOUTH)   // (10,10) is defined in the mock
      val engine = new Engine(emptyBoard, emptyGame.copy(plane = Some(plane)))
      val (_, g) = engine.run(explorer)
      g.isOK must beTrue
      g.plane.get.position must_!= plane.position
      g.budget.remaining must beLessThan(g.budget.initial)
    }

    "reject echoing in the wrong direction" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "echo", parameters: { "direction": "N" } }""" thenReturn stop
      val plane = Plane(13,7,Directions.SOUTH)   // (10,10) is defined in the mock
      val engine = new Engine(emptyBoard, emptyGame.copy(plane = Some(plane)))
      val (_, g) = engine.run(explorer)
      g.isOK must beFalse
    }

    "support echoing" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "echo", parameters: { "direction": "S" } }""" thenReturn stop
      val plane = Plane(10,10,Directions.SOUTH)   // (10,10) is defined in the mock
      val engine = new Engine(emptyBoard, emptyGame.copy(plane = Some(plane)))
      val (_, g) = engine.run(explorer)
      g.isOK must beTrue
      g.plane.get.position must_== plane.position
      g.budget.remaining must beLessThan(g.budget.initial)
    }

    "support scanning" in {
      val explorer = mock[IExplorerRaid]
      explorer.takeDecision() returns """{ "action": "scan" }""" thenReturn stop
      val plane = Plane(10,10,Directions.SOUTH)   // (10,10) is defined in the mock
      val engine = new Engine(emptyBoard, emptyGame.copy(plane = Some(plane)))
      val (_, g) = engine.run(explorer)
      g.isOK must beTrue
      g.plane.get.position must_== plane.position
      g.budget.remaining must beLessThan(g.budget.initial)
    }
  }

}
