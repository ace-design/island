package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import eu.ace_design.island.stdlib.PointOfInterests.Creek
import eu.ace_design.island.stdlib.Resources
import eu.ace_design.island.stdlib.Resources._
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TransformTest extends SpecificationWithJUnit {

  "Transform Action Specification".title

  val g = Game(Budget(800), Crew(50), Set((Resources.WOOD, 600)))
  val b = GameBoard(size = 600, m = null, pois = Map((0,0) -> Set(Creek(identifier = "c", location = None))))


  "The Transform action" should {

    val getSomeRum  = Transform(Map(SUGAR_CANE -> 102, FRUITS -> 15))    // should produced around 10 units, +/- 10%
    val getSomeRum2  = Transform(Map(SUGAR_CANE -> -100, FRUITS -> -100))    // should produced around 10 units, +/- 10%

    val onLand = exec(Seq(MovedBoatResult(loc = (0,0), men = 2)), g) // Mens are on land now

    "reject to build something with no mens on land" in {
      getSomeRum.buildResult(b, g) must throwAn[IllegalArgumentException]
    }

    "reject to build something without enough resources to do so" in {
      getSomeRum.buildResult(b, onLand) must throwAn[IllegalArgumentException]
    }

    "reject to build something without negative value" in {
      getSomeRum2.buildResult(b, onLand) must throwAn[IllegalArgumentException]
    }


    "reject to transform using an unknown recipe" in {
      val unknown = Transform(Map(WOOD -> 15, FUR -> 15))
      val g1 = onLand.harvest(WOOD, (0,0), 100).harvest(FUR,(0,0),100)
      unknown.buildResult(b,g1) must throwAn[IllegalArgumentException]
    }

    "transform resources according to recipes" in {
      val g1 = onLand.harvest(SUGAR_CANE, (0,0), 900).harvest(FRUITS,(0,0),50)
      val res = getSomeRum.buildResult(b, g1).asInstanceOf[TransformResult]
      res.consumed must_== getSomeRum.materials
      res.kind must_== RUM
      res.production must beGreaterThanOrEqualTo(9)
      res.production must beLessThanOrEqualTo(11)
      val cost = getSomeRum.computeCost(b, g1)
      cost must beGreaterThan(0.0)
    }

    "produce the minimum of what can be actually produced with the given resources" in {
      // Asking to transform GLASS with 1200 units of QUARTZ but only 10 WOODs will only yield ~2 units of GLASS
      // The remaining resources are wasted
      val stupidTransformation = Transform(Map(QUARTZ -> 1200, WOOD -> 10))
      val g1 = onLand.harvest(QUARTZ, (0,0), 1500).harvest(WOOD,(0,0),50)
      val res = stupidTransformation.buildResult(b, g1).asInstanceOf[TransformResult]
      res.consumed must_== stupidTransformation.materials
      res.kind must_== GLASS
      res.production must beGreaterThanOrEqualTo(1)
      res.production must beLessThanOrEqualTo(3)
      val cost = stupidTransformation.computeCost(b, g1)
      cost must beGreaterThan(0.0)
    }

    "adapt the game with the relevant resources after transformation" in {
      val result = TransformResult(kind = RUM, production = 1, consumed = getSomeRum.materials)
      val updated = exec(Seq(result), onLand.harvest(SUGAR_CANE, (0,0), 900).harvest(FRUITS,(0,0),50))
      updated.collectedResources must contain(SUGAR_CANE -> 798)
      updated.collectedResources must contain(FRUITS     -> 35)
      updated.collectedResources must contain(RUM        -> 1)
    }
  }

}
