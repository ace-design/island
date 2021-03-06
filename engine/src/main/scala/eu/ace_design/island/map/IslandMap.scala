package eu.ace_design.island.map

import eu.ace_design.island.geom._
import eu.ace_design.island.map.processes.Statistics

import scala.util.Random


/**
 * Companion object to hide the private constructor in the class with the apply syntactic sugar
 */
object IslandMap {
  def apply(mesh: Mesh) = new IslandMap(mesh)
}

/**
 * An IslandMap wraps a geometrical mesh and add properties (i.e., semantics) to each faces. It acts as the glue that
 * connect all the different elementary bricks (e.g., registries, property sets) into a consistent entity one can use
 * to properly handle an Island Map.
 *
 * The methods and values defined inside this class are simply "consistent calls" to the underlying APIs, adding some
 * business values to the elementary entities (thus DRYing the code).
 *
 * @param _mesh the geometrical mesh used
 * @param faceProps a propertySet associated to the faces stored in mesh
 * @param vertexProps a propertySet associated to the vertices stored in mesh
 * @param edgeProps a PropertySet associated to the edges stored in the mesh
 * @param uuid if given, it contains the UUID used to initialise the random generator
 * @param stats statistics about the map (optional)
 */
class IslandMap private (
    private val _mesh: Mesh,
    val uuid: Option[Long]       = None,
    val stats: Option[Map[Statistics.StatName, String]] = None,
    val faceProps: PropertySet   = PropertySet(),
    val vertexProps: PropertySet = PropertySet(),
    val edgeProps: PropertySet   = PropertySet()) {

  require(_mesh.size.isDefined, "A map must rely on a sized mesh")

  val size: Int = _mesh.size.get

  def mesh: Mesh = _mesh

  /**
   * Working with faces
   */
  val faces: Set[Face]      = _mesh.faces.values
  val faceRefs: Set[Int]    = _mesh.faces.references
  def face(i: Int): Face    = _mesh.faces(i)
  def faceRef(f: Face): Int = _mesh.faces(f).get
  def cornerRefs(f: Face): Set[Int] = f.vertices(_mesh.edges)

  // Compute the convex hull of a given face (for geometrical purpose)
  def convexHull(f: Face): Seq[Point] = {
    import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
    val coordinates = (cornerRefs(f) map { vertex } map { p => new Coordinate(p.x, p.y) }).toSeq
    val closed = coordinates :+ new Coordinate(coordinates(0).x, coordinates(0).y)
    val raw = new GeometryFactory().createPolygon(closed.toArray).convexHull.getCoordinates.toSeq
    raw map { c => Point(c.x, c.y) }
  }

  def findFacesWith: Set[Property[_]] => Set[Face]   = faceProps.project(_mesh.faces)
  def findFaceRefsWith(p: Face => Boolean): Set[Int] = _mesh.faces.queryReferences(p)

  /**
   * Working with vertices
   */
  val vertices: Set[Point]     = _mesh.vertices.values
  val vertexRefs: Set[Int]     = _mesh.vertices.references
  def vertex(i: Int): Point    = _mesh.vertices(i)
  def vertexRef(v: Point): Int = _mesh.vertices(v).get
  def neighbors(vRef: Int): Set[Int] = _mesh.edges.getAdjacencyFor(vRef)

  val findVerticesWith: Set[Property[_]] => Set[Point] = vertexProps.project(_mesh.vertices)
  def findVertexRefsWith(p: Point => Boolean) = _mesh.vertices.queryReferences(p)

  /**
   * Working with edges
   */
  val edges: Set[Edge]       = _mesh.edges.values
  val edgeRefs: Set[Int]     = _mesh.edges.references
  def edge(i: Int): Edge     = _mesh.edges(i)
  def edgeRef(e: Edge): Int  = _mesh.edges(e).get

  val findEdgesWith: Set[Property[_]] => Set[Edge] = edgeProps.project(_mesh.edges)


  /**
   * copy Method defined to mimic classical case classes (mesh cannot be updated)
   * @param faceProps
   * @param vertexProps
   * @param edgeProps
   * @param uuid
   * @param stats
   * @return
   */
  def copy(faceProps: PropertySet = this.faceProps, vertexProps: PropertySet = this.vertexProps,
           edgeProps: PropertySet = this.edgeProps, uuid: Option[Long] = this.uuid,
           stats: Option[Map[Statistics.StatName, String]] = this.stats): IslandMap =
    new IslandMap(this._mesh, uuid, stats, faceProps, vertexProps, edgeProps)

  /**
   * Structural equality for maps
   * @param other
   * @return
   */
  override def equals(other: Any) = other match {
    case that: IslandMap => this._mesh == that._mesh && this.faceProps == that.faceProps &&
                              this.vertexProps == that.vertexProps && this.edgeProps == that.edgeProps &&
                                this.uuid == that.uuid
    case _ => false
  }

  /**
   * HashCode delegated to the List algorithm
   * @return
   */
  override def hashCode(): Int = (_mesh :: faceProps :: vertexProps :: uuid :: Nil).hashCode()
}


