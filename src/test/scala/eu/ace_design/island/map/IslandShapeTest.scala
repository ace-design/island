package eu.ace_design.island.map

import eu.ace_design.island.geom.Point
import org.specs2.mutable._


class IslandShapeTest extends SpecificationWithJUnit {

  "IslandShapeTest Specifications".title

  val SIZE = 10
  val sample = for (x <- 0 until SIZE; y <- 0 until SIZE) yield Point(x.toDouble, y.toDouble)
  val center = Point(SIZE.toDouble/2, SIZE.toDouble / 2)

  "The Circular island" should {
    val radius = SIZE.toDouble / 2 * 0.5
    val shape = DiskShape(SIZE, radius)
    "accepts all points inside a circle" in {
      val expected = sample filter { p => center --> p > radius }
      val obtained = sample filter { p => shape.isWater(p)      }
      obtained must containTheSameElementsAs(expected)
    }
  }

  "The Donuts island" should {
    val externalRadius = SIZE.toDouble / 2 * 0.8
    val internalRadius = SIZE.toDouble / 2 * 0.25
    val shape = DonutShape(SIZE, externalRadius, internalRadius)
    "accept all points inside a " in {
      val expected = sample.filter { p =>
        (center --> p > externalRadius) || (center --> p < internalRadius)
      }
      val obtained = sample filter { p => shape.isWater(p) }
      obtained must containTheSameElementsAs(expected)
    }
  }

}