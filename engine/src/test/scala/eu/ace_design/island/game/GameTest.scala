package eu.ace_design.island.game

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameTest extends SpecificationWithJUnit {

  "GameTest Specifications".title

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


}