package eu.ace_design.island.dsl

import eu.ace_design.island.viewer.Viewer
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DiSLandTest extends SpecificationWithJUnit with DiSLand {

  "DiSLandTest Specifications".title

  "The percentage wrapper" should {
    "reject negative value" in { (-2).percent must throwAn[IllegalArgumentException] }
    "reject value greater than 100" in { 101.percent must throwAn[IllegalArgumentException] }
    "compute a double when asked for" in { 65.percent.value must_== 0.65  }
  }

  "The face wrapper" should {
    "transparently support the 'int.faces' construction" in { 600.faces must_== 600 }
  }

  "The output viewers" should {
    "provide the pdf keyword" in { pdf must beAnInstanceOf[Viewer] }
    "provide the json keyword" in { json must beAnInstanceOf[Viewer] }
    "provide the obj keyword" in { obj must beAnInstanceOf[Viewer] }
  }

}