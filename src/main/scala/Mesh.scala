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

  override def faces: Set[Face] =
    (for(i <- 0 until chunk) yield Face(center=Point(0,i))).toSet
}


/**
 * A Face is characterized by its "center"
 * @param center
 */
case class Face(center: Point)

/**
 * A point represents a location in a 2 dimensional plane
 * @param x
 * @param y
 */
case class Point(x: Float, y: Float)

