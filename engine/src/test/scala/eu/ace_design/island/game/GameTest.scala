package eu.ace_design.island.game

import eu.ace_design.island.stdlib.Resources.{FUR, WOOD}
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