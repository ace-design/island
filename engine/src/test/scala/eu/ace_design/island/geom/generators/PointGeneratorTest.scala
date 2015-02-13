package eu.ace_design.island.geom.generators

import eu.ace_design.island.geom.Point
import org.specs2.mutable._

/**
 * This file is part of the island project.
 * @author mosser (22/06/2014, 19:28)
 */

class PointGeneratorTest extends SpecificationWithJUnit {

  "PointGenerator specification".title

  final val GRID_SIZE: Int = 200

  "Any PointGenerator" should {
    val g = new PointGenerator {
      override def size: Int = ???
      override protected def run(n: Int): Set[Point] = ???
    }
    "reject a negative number of points to be generated" in {
      g(-1) must throwAn[IllegalArgumentException]
    }
  }

  "A SquaredGrid generator" should {
    val g = new SquaredGrid(GRID_SIZE)
    "accept its size as its constructor parameter" in { g.size must_== GRID_SIZE }
    "reject the generation of non-squared points" in {
      g(3) must throwAn[IllegalArgumentException]
    }
    "generates points according to a grid-based representation" in {
      val expected = Set(Point(50, 50), Point(150, 50), Point(50, 150), Point(150, 150))
      g(4) must_== expected
    }
  }

  def isOk(set: Set[Double]) = set foreach { elem =>
    elem must be_>=(0.0)
    elem must be_<=(GRID_SIZE.asInstanceOf[Double])

  }

  "A RandomGrid generator" should {
    val g = new RandomGrid(GRID_SIZE)
    "generate the appropriate number of points" in { g(4) must haveSize(4) }
    "generates points in the given space" in {
      val points = g(10)
      isOk(points map { _.x })
      isOk(points map { _.y })
      true must beTrue
    }
    "generate reproducible data sets" in {
      val seed = 123456789
      val g1 = new RandomGrid(GRID_SIZE, new scala.util.Random(seed))
      val g2 = new RandomGrid(GRID_SIZE, new scala.util.Random(seed))
      g1(10) must_== g2(10)
    }
  }

  "A RelaxedRandomGrid generator" should {
    val g = new RelaxedRandomGrid(GRID_SIZE)
    "generate the appropriate number of points" in { g(4) must haveSize(4) }
    "generates points in the given space" in {
      val points = g(10)
      isOk(points map { _.x })
      isOk(points map { _.y })
      true must beTrue
    }
    "generate reproducible data sets" in {
      val seed: Long = 1234567890
      val g1 = new RelaxedRandomGrid(size = GRID_SIZE, random = new scala.util.Random(seed))
      val g2 = new RelaxedRandomGrid(size = GRID_SIZE, random = new scala.util.Random(seed))
      g1(10) must_== g2(10)
    }
  }

}
