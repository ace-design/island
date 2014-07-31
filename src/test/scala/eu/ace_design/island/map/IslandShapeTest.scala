package eu.ace_design.island.map

import eu.ace_design.island.geom.Point
import org.specs2.mutable._


class IslandShapeTest extends SpecificationWithJUnit {

  "IslandShapeTest Specifications".title

  val SIZE = 50
  val sample = for (x <- 0 until SIZE; y <- 0 until SIZE) yield Point(x.toDouble, y.toDouble)
  val center = Point(SIZE.toDouble/2, SIZE.toDouble / 2)

  "The Disk island" should {
    val radius = SIZE.toDouble / 2 * 0.5
    val shape = DiskShape(SIZE, radius)

    "accept all points inside its circle" in {
      val expected = sample filter { p => center --> p > radius }
      val obtained = sample filter { p => shape.isWater(p)      }
      obtained must containTheSameElementsAs(expected)
    }
    "reject negative or null values for size and radius" in {
      DiskShape(-1,20) must throwAn[IllegalArgumentException]
      DiskShape(20,-1) must throwAn[IllegalArgumentException]
      DiskShape(0, 20) must throwAn[IllegalArgumentException]
      DiskShape(20, 0) must throwAn[IllegalArgumentException]
    }
  }

  "The Donuts island" should {
    val externalRadius = SIZE.toDouble / 2 * 0.8
    val internalRadius = SIZE.toDouble / 2 * 0.25
    val shape = DonutShape(SIZE, externalRadius, internalRadius)

    "accept all points inside an annulus" in {
      val expected = sample filter { p => (center --> p > externalRadius) || (center --> p < internalRadius) }
      val obtained = sample filter { p => shape.isWater(p) }
      obtained must containTheSameElementsAs(expected)
    }

    "reject negative or null values for size and the internal and external radius" in {
      DonutShape(-1,20,20) must throwAn[IllegalArgumentException]
      DonutShape(20,-1,20) must throwAn[IllegalArgumentException]
      DonutShape(20,20,-1) must throwAn[IllegalArgumentException]
      DonutShape(0, 20,20) must throwAn[IllegalArgumentException]
      DonutShape(20,0, 20) must throwAn[IllegalArgumentException]
      DonutShape(20,20, 0) must throwAn[IllegalArgumentException]
    }

    "reject an internal radius greater than the external one" in {
      DonutShape(300, 75, 100) must throwAn[IllegalArgumentException]
    }
  }

}