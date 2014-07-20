package eu.ace_design.island.geom


import eu.ace_design.island.util.Log
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
class MeshBuilder(val size: Int) extends Log {
  import com.vividsolutions.jts.geom.{CoordinateFilter, Polygon, GeometryCollection}

  /**
   * Create a Mesh by applying a builder to a given set of points
   * @param sites the points used to generate the mesh
   * @return the associated mesh
   */
  def apply(sites: Set[Point]): Mesh = {
    // Create an initial registry with the given sites
    val initialRegistry = (sites.par foldLeft VertexRegistry()) ( (reg, p) => reg + p )

    // introduce points added by the computation of the Voronoi diagram for this site
    val voronoiMesh = this.voronoi(sites, initialRegistry) clip size

    // Update the Face information to include neighborhood relationships (i.e., build a new face registry)
    val faceWithNeighbors = (FaceRegistry() /: voronoiMesh.faces.contents) { case (reg, (f, idx)) =>
      reg + f.copy(neighbors = Some(voronoiMesh.neighbors(idx)))
    }
    voronoiMesh.copy(faces = faceWithNeighbors)
  }

  /**
   * Exploit a Voronoi diagram to build the different area of the maps
   * @param sites a distribution of points used as inputs for the Voronoi Builder
   * @param vReg the initial vertex Registry containing the sites
   * @return a complete mesh (based on voronoi algorithm) with the associated Faces, Edges and Vertex.
   */
  private def voronoi(sites: Set[Point], vReg: VertexRegistry): Mesh = {
    import com.vividsolutions.jts.geom.{Coordinate, GeometryCollection, GeometryFactory}
    import com.vividsolutions.jts.triangulate.VoronoiDiagramBuilder
    import scala.collection.JavaConversions._

    // Transform the Points into JTS coordinates
    val coordinates = sites map { p => new Coordinate(p.x, p.y) }

    // Instantiate a DiagramBuilder, associated to the computed coordinates.
    logger.info("Generating Voronoi diagram")
    val builder = new VoronoiDiagramBuilder()
    builder.setSites(coordinates)
    //builder.setClipEnvelope(new Envelope(0,size,0,size))
    val geometry = builder.getDiagram(new GeometryFactory()).asInstanceOf[GeometryCollection]

    logger.info("Restricting the geometry to the map")
    // Bring back points to the map, breaking the Voronoi property for the boundary polygons
    geometry.apply(stayInTheBox)

    // Retrieve the Polygons contained in the diagram
    val polygons = for(i <- 0 until geometry.getNumGeometries)
                    yield geometry.getGeometryN(i).asInstanceOf[Polygon]

    // Compute the contents of the mesh
    val completeVertexRegistry = buildVertexRegistry(polygons, vReg)
    val edgeRegistry = buildEdgeRegistry(polygons, completeVertexRegistry)
    val faceRegistry = buildFaceRegistry(polygons, sites, completeVertexRegistry, edgeRegistry)
    // Return the mesh
    Mesh(vertices = completeVertexRegistry, edges = edgeRegistry, faces = faceRegistry)
  }


  private def buildPolygons(coll: GeometryCollection): Seq[Polygon] = ???

  /**
   * Compute a vertex registry that contains all the vertices used in the given polygons
   * @param polygons the polygons to work on
   * @param init a Vertex Registry used as starting point
   * @return  A vertex registry containing all the vertices in init + the one defined in the given polygons
   */
  private def buildVertexRegistry(polygons: Seq[Polygon], init: VertexRegistry): VertexRegistry = {
    logger.info("Building VertexRegistry")
    (init /: polygons.par) { (r, poly) =>
      val coordinates = poly.getBoundary.getCoordinates
      val points = coordinates map { c => Point(c.x, c.y) }
      points.foldLeft(r) { (acc, point) => acc + point }
    }
  }

  /**
   * Compute and EdgeRegistry based on the given polygons and a vertex registry containing the associated vertices
   * @param polygons the polygons to work on
   * @param vertices the vertices used by these polygons
   * @return the associated EdgeRegistry
   */
  private def buildEdgeRegistry(polygons: Seq[Polygon], vertices: VertexRegistry): EdgeRegistry = {
    logger.info("Building EdgeRegistry")
    (EdgeRegistry() /: polygons.par) { (r, poly) =>
      val edges = extractEdges(vertices, poly)
      edges.foldLeft(r) { (reg, e) => reg + e }
    }
  }

  /**
   * Compute a FaceRegistry based on the given polygons, the sites used to compute these polygons,
   * and preexisting registries
   * @param polygons the polygons to work on
   * @param sites the sites used to generate the polygons
   * @param vReg the vertexRegistry used to store the vertices
   * @param eReg the edgeRegistry used to store the edges
   * @return a FaceRegistry
   */
  private def buildFaceRegistry(polygons: Seq[Polygon], sites: Set[Point],
                                vReg: VertexRegistry, eReg: EdgeRegistry): FaceRegistry = {
    logger.info("Building Face Registry")
    (FaceRegistry() /: polygons.par) { (reg, poly) =>
      val centerRef = extractCenter(sites, vReg, poly)
      val edgeRefs = extractEdges(vReg, poly) map { e => eReg(e).get }
      reg + Face(center = centerRef, edges = edgeRefs)
    }
  }

  /**
   * Transform a given polygon into a sequence of Edges
   * @param vReg the VertexRegistry containing the associated vertices
   * @param poly the polygon to transform
   * @return the associated sequence of edges
   */
  private def extractEdges(vReg: VertexRegistry, poly: Polygon): Seq[Edge] = {
    logger.trace(s"Extracting edges for $poly")
    def loop(points: Array[Point]): Seq[Edge] = points match {
      case Array() => Seq()
      case Array(p) => Seq()
      case Array(p1, p2, _*) => Edge(vReg(p1).get, vReg(p2).get) +: loop(points.slice(1, points.length))
    }
    loop(poly.getBoundary.getCoordinates map { c => Point(c.x, c.y) })
  }

  /**
   * For a given polygon, find the voronoi site used to compute it
   * @param sites  the sites used to build the polygon according to the Voronoi algorithm
   * @param vReg the VertexRegistry containing
   * @param poly
   * @return
   */
  private def extractCenter(sites: Set[Point], vReg: VertexRegistry, poly: Polygon): Int = {
    logger.trace(s"Extracting center for $poly")
    val geomFact = new com.vividsolutions.jts.geom.GeometryFactory()
    val opt = sites.par find { p =>
      poly.contains(geomFact.createPoint(new com.vividsolutions.jts.geom.Coordinate(p.x, p.y)))
    }
    opt match {
      case Some(p) => vReg(p) match {
        case Some(pRef) => pRef
        case None => throw new IllegalArgumentException("Center is not contained in the VertexRegistry")
      }
      case None => throw new IllegalArgumentException("No site contained by this polygon")
    }
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
