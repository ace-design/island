package eu.ace_design.island.geom.generators

import eu.ace_design.island.geom.Point
import scala.util.Random

/**
 * This generator generates a set of relaxed pseudo-random points, according to a standard uniform distribution law
 *
 * The smoothing is done according to the following techniques:
 * Generate a set of Random points. For a given number of time, compute the Voronoi diagram of the distribution,
 * and move each point to the centroid of its associated polygon.
 *
 * Remark: Without any smoothing iteration, this generator has the same behavior as the RandomGrid one.  This generator
 * require the JTS Topology Suite to compute Voronoi diagrams and centroid coordinates.
 *
 * @param size   the size of the PointGenerator
 * @param factor the number of iteration executed for smoothing (default is 5)
 * @param random the random generator to be used (by default, instantiating a new one)
 */
class RelaxedRandomGrid(override val size: Int,
                        override val random: Random = new Random(),
                        val factor: Int = 5) extends RandomPointGenerator {

  import com.vividsolutions.jts.geom.{Coordinate, CoordinateFilter, GeometryCollection, GeometryFactory}
  import com.vividsolutions.jts.triangulate.VoronoiDiagramBuilder
  import scala.collection.JavaConversions._
  import scala.math.{max, min}


  override protected def run(n: Int): Set[Point] = {
    val distribution = for (i <- 1 to n) yield Point(random.nextDouble() * size, random.nextDouble() * size)
    smooth(distribution, factor).toSet
  }

  private def smooth(points: Seq[Point], n: Int): Seq[Point] = {
    require(n >= 0, "n must be positive or null")
    n match {
      case 0 => points
      case x => {
        val coordinates = points map { p => new Coordinate(p.x, p.y)}
        val voronoi = new VoronoiDiagramBuilder()
        voronoi.setSites(coordinates)
        val geometry = voronoi.getDiagram(new GeometryFactory()).asInstanceOf[GeometryCollection]
        geometry.apply(stayInTheBox)
        val centroids = for (i <- 0 until geometry.getNumGeometries)
                            yield geometry.getGeometryN(i).getCentroid.getCoordinate
        val result = centroids map { c => Point(inside(c.x), inside(c.y)) }
        smooth(result, n - 1)
      }
    }
  }

  // Returns a double that is equals to 0 is d < 0, equals to size if d > size, or d elsewhere.
  private def inside(d: Double): Double = min(max(d,0.0),size)

  // This coordinate filter helps the voronoi diagram to stay in the defined clipping space (size x size)
  private val stayInTheBox = new CoordinateFilter {
    override def filter(c: Coordinate) { c.setCoordinate(new Coordinate(inside(c.x), inside(c.y))) }
  }

}
