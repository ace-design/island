package eu.ace_design.island.geom

import eu.ace_design.island.RelaxedRandomGrid
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

/**
 * This file is part of the island project
 * @author mosser (22/06/2014, 19:28)
 **/

@RunWith(classOf[JUnitRunner])
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

  "A RandomGrid generator" should {
    val g = new RandomGrid(GRID_SIZE)
    "generate the appropriate number of points" in { g(4) must haveSize(4) }
    "generates points in the given space" in {
      val points = g(10)
      val xs: Set[Double] = points map { _.x }
      xs must contain(be_>=(0.0)).forall
      xs must contain ( be_<=(GRID_SIZE.asInstanceOf[Double]) ).forall
      val ys: Set[Double] = points map { _.y }
      ys must contain ( be_>=(0.0) ).forall
      ys must contain ( be_<=(GRID_SIZE.asInstanceOf[Double]) ).forall
    }
  }

  "A RelaxedRandomGrid generator" should {
    val g = new RelaxedRandomGrid(GRID_SIZE)
    "generate the appropriate number of points" in { g(4) must haveSize(4) }
    "generates points in the given space" in {
      val points = g(10)
      val xs: Set[Double] = points map { _.x }
      xs must contain(be_>=(0.0)).forall
      xs must contain ( be_<=(GRID_SIZE.asInstanceOf[Double]) ).forall
      val ys: Set[Double] = points map { _.y }
      ys must contain ( be_>=(0.0) ).forall
      ys must contain ( be_<=(GRID_SIZE.asInstanceOf[Double]) ).forall
    }
  }

}
