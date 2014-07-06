package eu.ace_design.island


import eu.ace_design.island.geom._

/**
 * This file is part of the Island project.
 * @author mosser
 **/

/**
 * A MeshBuilder is used to generate an Island mesh based on a given set of Points.
 *
 * Remark: this class exploits the JTS Topology Suites for Voronoi and Delaunay computations
 *
 * @param size the size of the map (a square of size x size)
 */
class MeshBuilder(val size: Int) {
  import com.vividsolutions.jts.geom.CoordinateFilter
  import com.vividsolutions.jts.geom.Polygon

  /**
   * Create a Mesh by applying a builder to a given set of points
   * @param sites the points used to generate the mesh
   * @return the associated mesh
   */
  def apply(sites: Set[Point]): Mesh = {
    // Create an initial registry with the given sites
    val initialRegistry = (sites foldLeft VertexRegistry()) ( (reg, p) => reg + p )
    val initialMesh = Mesh(vertices = initialRegistry)

    // introduce points added by the computation of the Voronoi diagram for this site
    val voronoiMesh = this.voronoi(sites, initialMesh)

    voronoiMesh
  }


  private def voronoi(sites: Set[Point], mesh: Mesh): Mesh = {
    import scala.collection.JavaConversions._
    import com.vividsolutions.jts.triangulate.VoronoiDiagramBuilder
    import com.vividsolutions.jts.geom.{Coordinate, GeometryCollection, GeometryFactory}

    /** Exploiting JTS to compute the geometrical objects associated to **/

    // Transform the Points into JTS coordinates
    val coordinates = sites map { p => new Coordinate(p.x, p.y) }
    // Instantiate a DiagramBuilder, associated to the computed coordinates.
    val builder = new VoronoiDiagramBuilder()
    builder.setSites(coordinates)
    // Actually compute the diagram
    val geometry = builder.getDiagram(new GeometryFactory()).asInstanceOf[GeometryCollection]
    // Bring back points to the map
    geometry.apply(stayInTheBox)
    // Retrieve the Polygons contained in the diagram
    val polygons = for(i <- 0 until geometry.getNumGeometries) yield geometry.getGeometryN(i).asInstanceOf[Polygon]

    /** add the points located in the polygons to the received registry  **/
    val vertexRegistry = polygons.foldLeft(mesh.vertices) { (r, poly) =>
      val coordinates = poly.getBoundary.getCoordinates
      val points = coordinates map { c => Point(c.x, c.y) }
      points.foldLeft(r) { (acc, point) => acc + point }
    }
    val meshWithVertices = mesh + vertexRegistry

    /** add the different edges stored in each polygon **/
    val edgeRegistry = polygons.foldLeft(EdgeRegistry()) { (r, poly) =>
      val edges = extractEdges(meshWithVertices.vertices, poly)
      edges.foldLeft(r) { (reg, e) => reg + e }
    }

    val meshWithVerticesAndEdges = meshWithVertices + edgeRegistry

    /** Return the mesh based on the voronoi representation (vertices, edges and faces)**/
    meshWithVerticesAndEdges
  }

  private def extractEdges(vReg: VertexRegistry, poly: Polygon): Seq[Edge] = {
    def loop(points: Array[Point]): Seq[Edge] = points match {
      case Array() => Seq()
      case Array(p) => Seq()
      case Array(p1, p2, _*) => Edge(vReg(p1).get,vReg(p2).get) +: loop(points.slice(1,points.length))
    }
    loop(poly.getBoundary.getCoordinates map { c => Point(c.x, c.y) })
  }

  /**
   * Helper anonymous class to keep the geometrical computation inside the map
   */
  val stayInTheBox = new CoordinateFilter {
    import com.vividsolutions.jts.geom.Coordinate
    import scala.math._

    /**
     * Check if a point is located inside the map (size x size square).
     * @param d the point to check
     * @return the point if true, a border one (out-of-the-box coordinate set to 0) elsewhere
     */
    private def inside(d: Double): Double = min(max(d,0.0),size)

    /**
     * The filter method is defined by the CoordinateFilter Interface. It is applied to each Coordinate of a Geometry.
     * This filter applies the "inside" function to each point involved in a geometry
     * @param c
     */
    override def filter(c: Coordinate) { c.setCoordinate(new Coordinate(inside(c.x), inside(c.y))) }
  }
}
