package eu.ace_design.island.map.processes

import org.specs2.mutable._

class AssignMoistureTest extends SpecificationWithJUnit {

  "AssignMoistureTest Specifications".title



  "The MoisturePropagation function library" should {

    val fSqrt = MoisturePropagation(MoisturePropagation.orderSqrt) _
    val f1 = MoisturePropagation(MoisturePropagation.order1) _
    val f2 = MoisturePropagation(MoisturePropagation.order2) _
    val f3 = MoisturePropagation(MoisturePropagation.order3) _
    val f4 = MoisturePropagation(MoisturePropagation.order4) _

    "assign 100 for a point identified as fresh water (distance = 0)" in {
      fSqrt(0) must_== 100; f1(0) must_== 100; f2(0) must_== 100; f3(0) must_== 100; f4(0) must_== 100
    }

    "assign 0 to points located at 50+ the source" in {
      fSqrt(50) must_== 0; f1(50) must_== 0; f2(50) must_== 0; f3(50) must_== 0; f4(50) must_== 0
    }

    "reject negative distance" in {
      fSqrt(-1) must throwAn[IllegalArgumentException]
      f1(-1)    must throwAn[IllegalArgumentException]
      f2(-1)    must throwAn[IllegalArgumentException]
      f3(-1)    must throwAn[IllegalArgumentException]
      f4(-1)    must throwAn[IllegalArgumentException]
    }
  }

}