package eu.ace_design.island.game

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ResultTest extends SpecificationWithJUnit {

  "ResultTest Specifications".title

  "An empty result" should {
    "support cost update" in {
      val empty = EmptyResult()
      empty.cost must_== 0
      val after = empty withCost 30
      after.cost must_== 30
    }
  }

}