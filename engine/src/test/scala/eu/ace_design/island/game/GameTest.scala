package eu.ace_design.island.game

import eu.ace_design.island.stdlib.Resources._
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameTest extends SpecificationWithJUnit {

  "GameTest Specifications".title

  "A game" should {
    val g = Game(budget = Budget(100), crew = Crew(50), objectives = Set((WOOD, 600)))

    "be instantiated like a case class" in {
      g must beAnInstanceOf[Game]
      g.visited must_== Set()
      g.boat must beNone
      g.isOK must beTrue
    }

    "support harvesting of resources" in {
      g.harvested(WOOD, (0,0)) must_== 0
      val g2 = g.harvest(WOOD, (0,0), 100)
      g2.harvested(WOOD, (0,0)) must_== 100
      val g3 = g2.harvest(WOOD, (0,0), 2).harvest(WOOD, (0,1), 100).harvest(FLOWER, (0,0), 20)
      g3.harvested(WOOD, (0,1)) must_== 100
      g3.harvested(WOOD, (0,0)) must_== 102
      g3.harvested(FLOWER, (0,0)) must_== 20
    }

    "compute the distance to go back to the home port" in {
      g.distanceToPort must beNone
      val (g1,_) = g updatedBy MovedBoatResult(loc = (10,10), men = 10)
      g1.distanceToPort must beSome
      g1.distanceToPort.get must beGreaterThan(0.0)
    }

    "compute the distance to reach the boat when on the island" in {
      g.distanceToBoat must_== 0.0 // everybody on the boat
      val (g1,_) = g updatedBy MovedBoatResult(loc = (10,10), men = 10)
      g1.distanceToBoat must_== 0.0 // still close to the boat
      val (g2,_) = g1 updatedBy MovedCrewResult(loc = (10,11))
      g2.distanceToBoat must beGreaterThan(0.0)
    }

    "support the projection of landed mens into something much smaller (costs models)" in {
      val (g1,_) = g updatedBy MovedBoatResult(loc = (10,10), men = 10)
      g1.menRatio must beLessThan(10.0)
    }

    "support normalization of landed mens (projection into [0,1]" in {
      g.normalizeMen must_== 0.0
      val (g1,_) = g updatedBy MovedBoatResult(loc = (10,10), men = 10)
      g1.normalizeMen must beLessThan(1.0)
    }

    "compute the distance to move the boat from one place to another one" in {
      g.distanceByBoat((10,10)) must beGreaterThan(0.0)
    }

    "be flagged as KO when relevant"  in {
      val g1 = g.flaggedAsKO
      g.isOK  must beTrue
      g1.isOK must beFalse
    }
    "support update with an EmptyResult" in {
      val r = EmptyResult(cost = 20)
      val (updated, _) = g updatedBy r
      updated.budget.remaining must_== 80
    }
    "support update with a MoveBoatResult" in {
      val r = MovedBoatResult(cost = 30, loc = (10,14), men = 20)
      val (updated, _) = g updatedBy r
      updated.budget.remaining must beLessThan(g.budget.remaining)
      updated.boat must_== Some((10,14))
      updated.crew.landed must_== 20
    }
    "support harvest update" in {
      g.harvested(WOOD, (0,0)) must_== 0
      val g1 = g.harvest(WOOD, (0,0), 100)
      g1.harvested(WOOD, (0,0)) must_== 100
      val g2 = g.harvest(WOOD, (0,0), 25)
      g2.harvested(WOOD, (0,0)) must_== 25
      val g3 = g.harvest(FUR, (0,0), 25)
      g3.harvested(FUR, (0,0)) must_== 25
    }

    "support the consumption of resources" in {
      g.consumeResource(WOOD, 10) must throwAn[IllegalArgumentException]
      val g1 = g.harvest(WOOD, (0,0), 100)
      g1.collectedResources must contain(WOOD -> 100)
      val g2 = g1.consumeResource(WOOD, 90)
      g2.collectedResources must contain(WOOD -> 10)
      g2.consumeResource(WOOD, 20) must throwAn[IllegalArgumentException]
      val g3 = g2.consumeResource(WOOD, 10)
      g3.collectedResources must contain(WOOD -> 0)
    }

    "support the storage of manufactured resources in the ship hold" in {
      val g1 = g.storeTransformedResources(PLANK, 10)
      g1.collectedResources must contain(PLANK -> 10)

      val g2 = g1.storeTransformedResources(INGOT, 5)
      g2.collectedResources must contain(PLANK -> 10)
      g2.collectedResources must contain(INGOT -> 5)

      val g3 = g2.storeTransformedResources(PLANK, 32)
      g3.collectedResources must contain(PLANK -> 42)
      g3.collectedResources must contain(INGOT -> 5)
    }

  }

  "A budget" should {
    "reject negative or null initial value" in {
      Budget(-1) must throwAn[IllegalArgumentException]
      Budget(0)  must throwAn[IllegalArgumentException]
    }
    "support action point spending in a functional way" in {
      val init = Budget(100)
      val remains = init - 40
      init.initial must_== 100;    init.remaining must_== 100
      remains.initial must_== 100; remains.remaining must_== 60
    }
    "throw an exception when spending too much action points" in {
      val b = Budget(40)
      (b - 42) must throwA[NotEnoughBudgetException]
    }

  }

  "A crew" should {
    "reject crew with less than 2 men operating the boat" in {
      Crew(-1) must throwAn[IllegalArgumentException]
      Crew(0)  must throwAn[IllegalArgumentException]
      Crew(1)  must throwAn[IllegalArgumentException]
    }
    "be initialized with default values" in {
      val crew = Crew(50)
      crew.complete must_== 50
      crew.landed must_== 0
      crew.used must_== 0
      crew.location must_== None
    }
    "log how men are used in the crew" in {
      val crew = Crew(50)
      crew must beAnInstanceOf[Crew]
      crew.complete must_== 50; crew.landed must_== 0; crew.used must_== 0
      val c1 = crew using 15
      c1.complete must_== crew.complete; c1.landed must_== 15; c1.used must_== 15
      val c2 = c1 using 5
      c2.complete must_== crew.complete; c2.landed must_== 5; c2.used must_== 20
    }
    "know where the men who landed on the island are" in {
      val crew = Crew(50)
      val c1 = crew movedTo (14,17)
      c1.location must_== Some((14,17))
    }
  }
}