
import eu.ace_design.island.geom.Point

import scala.math._

/**
 * A trait to represent what is a mesh in Island
 * @author mosser
 */
trait Mesh {

  // The size of the plane used to build the mesh
  val size: Int

  // the number of faces expected as an output
  val chunk: Int

  // the faces computed by the mesh generation process
  def faces: Set[Face]

}

/**
 * A mesh implementation, using a squared
 * @param size
 * @param chunk
 */
class SquaredMesh(override val size: Int, override val chunk: Int) extends Mesh {
  require(chunk > 0, "Chunk must be positive")
  require(round(sqrt(chunk)) * round(sqrt(chunk)) == chunk ,"Chunk must be a squared number")
  require(size > 0, "Plane size must be positive")

  override val faces: Set[Face] = {
    val inc = sqrt(chunk).toInt
    val squareLength = size / sqrt(chunk)
    val delta = squareLength / 2

    val centers = for (x <- 0 until inc; y <- 0 until inc)
      yield Point(x * squareLength + delta, y * squareLength + delta)

    val res = centers map { p =>
      val ne = Point(p.x + delta, p.y - delta)  // North East
      val nw = Point(p.x - delta, p.y - delta)  // North West
      val se = Point(p.x + delta, p.y + delta)  // South East
      val sw = Point(p.x - delta, p.y + delta)  // South West
      Face(center = p, corners = Seq(nw, ne, se, sw))
    }

    res.toSet
  }
}


/**
 * A Face is characterized by its "center"
 * @param center
 */
case class Face(center: Point, corners: Seq[Point])



