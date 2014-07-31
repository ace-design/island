package eu.ace_design.island.geom


import eu.ace_design.island.util.{LogSilos, Logger}
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
class MeshBuilder(val size: Int) extends Logger {
  val silo = LogSilos.MESH_GEN

  import com.vividsolutions.jts.geom.{Polygon,  GeometryCollection}

  /**
   * Create a Mesh by applying a builder to a given set of points
   * @param sites the points used to generate the mesh
   * @return the associated mesh
   */
  def apply(sites: Set[Point]): Mesh = {
    // introduce points added by the computation of the Voronoi diagram for this site
    val voronoiMesh = this.voronoi(sites) clip size
    val mesh = buildDelaunayNeighborhood(voronoiMesh)
    mesh
  }

  /**
   * Exploit a Voronoi diagram to build the different area of the maps
   * @param sites a distribution of points used as inputs for the Voronoi Builder
   * @return a complete mesh (based on voronoi algorithm) with the associated Faces, Edges and Vertex.
   */
  private def voronoi(sites: Set[Point]): Mesh = {
    import com.vividsolutions.jts.geom.{Coordinate, GeometryCollection, GeometryFactory}
    import com.vividsolutions.jts.triangulate.VoronoiDiagramBuilder
    import scala.collection.JavaConversions._

    // Transform the Points into JTS coordinates
    val coordinates = sites.par map { p => new Coordinate(p.x, p.y) }

    // Instantiate a DiagramBuilder, associated to the computed coordinates.
    info("Generating Voronoi diagram")
    val builder = new VoronoiDiagramBuilder()
    builder.setSites(coordinates.seq)
    //builder.setClipEnvelope(new Envelope(0,size,0,size))
    val polygons =  buildPolygons(builder.getDiagram(new GeometryFactory()).asInstanceOf[GeometryCollection])

    // Compute the contents of the mesh
    val vertexRegistry = buildVertexRegistry(polygons)
    val edgeRegistry = buildEdgeRegistry(polygons, vertexRegistry)
    val faceRegistry = buildFaceRegistry(polygons, vertexRegistry, edgeRegistry)
    // Return the mesh
    Mesh(vertices = vertexRegistry, edges = edgeRegistry, faces = faceRegistry)
  }

  /**
   * Compute a sequence of Polygons based on a GeometryCollection obtained as the output of a Voronoi Builder
   * It aims to restricts the geometry to coordinates compatible with the Mesh to be built
   * @param geometry the output of a voronoi builder
   * @return a sequence of polygons compatible with a Mesh (\in [0, SIZE] x [0,SIZE])
   */
  private def buildPolygons(geometry: GeometryCollection): Seq[Polygon] = {
    import com.vividsolutions.jts.geom.Coordinate
    info("Building polygons")
    val rect = geometry.getFactory.createPolygon(Array(new Coordinate(0,0), new Coordinate(0,size),
                                                       new Coordinate(size, size), new Coordinate(size, 0),
                                                       new Coordinate(0,0)))
    val polySeq = geometryCollectionToPolygonSequence(geometry)
    (polySeq.par map { _.intersection(rect).asInstanceOf[Polygon] }).seq
  }

  /**
   * Transform a GeometryCollection into a sequence of polygons
   * @param g the collection to transform
   * @return an associated sequence of Polygons
   */
  private def geometryCollectionToPolygonSequence(g: GeometryCollection): Seq[Polygon] = {
    val iterator = (0 until g.getNumGeometries).par
    val r = (Seq[Polygon]() /: iterator) { (polygons, idx) =>
      val p: Polygon = g.getGeometryN(idx).asInstanceOf[Polygon]
      polygons :+ p
    }
    r
  }

  /**
   * Compute a vertex registry that contains all the vertices used in the given polygons
   * @param polygons the polygons to work on
   * @return  A vertex registry containing all the vertices in init + the one defined in the given polygons
   */
  private def buildVertexRegistry(polygons: Seq[Polygon]): VertexRegistry = {
    info("Building VertexRegistry")
    (VertexRegistry() /: polygons.par) { (r, poly) =>
      val coordinates = poly.getBoundary.getCoordinates
      val points = coordinates map { c => Point(c.x, c.y) }
      // We add the points located in the border + its centroid (to be used as the center of the associated face)
      (r /: points) { (acc, point) => acc + point } + getCentroid(poly)
    }
  }

  /**
   * Compute as a Point the centroid of a JTS polygon
   * @param p the polygon to handle
   * @return a Point
   */
  def getCentroid(p: Polygon): Point = {
    val tmp = p.getCentroid
    Point(tmp.getX, tmp.getY)
  }

  /**
   * Compute and EdgeRegistry based on the given polygons and a vertex registry containing the associated vertices
   * @param polygons the polygons to work on
   * @param vertices the vertices used by these polygons
   * @return the associated EdgeRegistry
   */
  private def buildEdgeRegistry(polygons: Seq[Polygon], vertices: VertexRegistry): EdgeRegistry = {
    info("Building EdgeRegistry")
    (EdgeRegistry() /: polygons.par) { (r, poly) =>
      val edges = extractEdges(vertices, poly)
      edges.foldLeft(r) { (reg, e) => reg + e }
    }
  }

  /**
   * Compute a FaceRegistry based on the given polygons, the sites used to compute these polygons,
   * and preexisting registries
   * @param polygons the polygons to work on
   * @param vReg the vertexRegistry used to store the vertices
   * @param eReg the edgeRegistry used to store the edges
   * @return a FaceRegistry
   */
  private def buildFaceRegistry(polygons: Seq[Polygon], vReg: VertexRegistry, eReg: EdgeRegistry): FaceRegistry = {
    info("Building Face Registry")
    (FaceRegistry() /: polygons.par) { (reg, poly) =>
      val centerRef = vReg(getCentroid(poly)).get
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
    trace(s"Extracting edges for $poly")
    def loop(points: Array[Point]): Seq[Edge] = points match {
      case Array() => Seq()
      case Array(p) => Seq()
      case Array(p1, p2, _*) => Edge(vReg(p1).get, vReg(p2).get) +: loop(points.slice(1, points.length))
    }
    loop(poly.getBoundary.getCoordinates map { c => Point(c.x, c.y) })
  }

  /**
   * Implements the computation of the neighborhood relationship between faces by leveraging a Delaunay triangulation.
   * It is actually a good enough method to compute neighbors (wrong for border polygons, but we don't care as
   * we are not using such polygons in our Island - always ocean).
   * @param mesh the mesh to be used to compute the neighborhood relations
   * @return a new mesh (faces updated to store their neighbors as an immutable set of face references)
   */
  def buildDelaunayNeighborhood(mesh: Mesh): Mesh  = {
    import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder
    import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
    import scala.collection.JavaConversions._

    def buildTriangles(m: Mesh): Seq[Polygon] = {
      info("Building Delaunay triangulation")
      val sites = m.faces.values.par map { f =>
        val center = m.vertices(f.center)
        new Coordinate(center.x, center.y)
      }
      val builder = new DelaunayTriangulationBuilder()
      builder.setSites(sites.seq)
      val geom = builder.getTriangles(new GeometryFactory()).asInstanceOf[GeometryCollection]
      geometryCollectionToPolygonSequence(geom)
    }

    type Neighborhood =  Map[Int, Set[Int]]

    def addToNeighbors(key: Int, neighbor: Int, data: Neighborhood): Neighborhood = data.get(key) match {
      case None => data + (key -> Set(neighbor))
      case Some(existing) => data - key + (key -> (existing + neighbor))
    }

    val triangles: Seq[Polygon] = buildTriangles(mesh)
    val emptyNeighborhood: Neighborhood = Map()
    info("Transforming the Delaunay triangulation into neighborhood relation")
    val neighborhood = (emptyNeighborhood /: triangles.par) { (acc, t) =>
      // polygons are "closed", thus the start point is a duplicate of the last one (=> distinct is used)
      val centerRefs = (t.getCoordinates map { c => mesh.vertices(Point(c.x, c.y)).get }).distinct
      // We transform these center references into faces
      val faceRefs = centerRefs map { mesh.faces.lookFor(_).get}
      // we build the neighborhood pairs based on the triangle contents
      val pairs = for(i <- faceRefs; j<- faceRefs) yield (i,j)
      // we update the accumulator wit the pairs
      (acc /: pairs) { (res,p) => addToNeighbors(p._1, p._2, res) }
    }

    info("Updating the FaceRegistry with the neighborhood relation")
    val updatedFaces = (mesh.faces /: neighborhood.par) { (reg, info) =>
      val face = reg(info._1).copy(neighbors = Some(info._2))
      reg.update(info._1, face)
    }
    mesh.copy(faces = updatedFaces)
  }
}
