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
    logger.info("Generating Voronoi diagram")
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
    logger.info("Building polygons")
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
    logger.info("Building VertexRegistry")
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
   * @param vReg the vertexRegistry used to store the vertices
   * @param eReg the edgeRegistry used to store the edges
   * @return a FaceRegistry
   */
  private def buildFaceRegistry(polygons: Seq[Polygon], vReg: VertexRegistry, eReg: EdgeRegistry): FaceRegistry = {
    logger.info("Building Face Registry")
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
    logger.trace(s"Extracting edges for $poly")
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

    logger.info("Building Delaunay triangulation")
    val builder = new DelaunayTriangulationBuilder()
    val sites = mesh.faces.values.par map { f =>
      val center = mesh.vertices(f.center)
      new Coordinate(center.x, center.y)
    }
    builder.setSites(sites.seq)
    val geom = builder.getTriangles(new GeometryFactory()).asInstanceOf[GeometryCollection]
    val triangles: Seq[Polygon] = geometryCollectionToPolygonSequence(geom)

    logger.info("Transforming the Delaunay triangulation into neighborhood relation")

    // Propagate i in xs. For i = 1 and xs = (4,5,6), produces Seq( (1,4), (1,5), (1,6) )
    def handle(i: Int, xs: Seq[Int]): Seq[(Int,Int)] = xs match {
      case Seq() => Seq()
      case Seq(y, ys@_*) => (i -> y) +: handle(i, ys)
    }

    // Merge a given sequence of map, grouping the references according to the keys (1 -> (2) + 1 -> (3) => 1 -> (2,3))
    def merge(data: Seq[Map[Int,Set[Int]]]): Map[Int, Set[Int]] = {
      (data map { _.toList }).flatten.groupBy { _._1 } map { p => p._1 -> (p._2 map { e => e._2}).flatten.toSet}
    }

    // transforming the set of triangles into neighborhood relationship
    val raw = triangles.map { t =>
      // We retrieve the associated vertex (coordinates points to faces centers)
      val faceRefs = (t.getCoordinates map { c => mesh.vertices(Point(c.x, c.y)).get }).distinct
      // We transform these center references into faces
      val faces = faceRefs map { mesh.faces.lookFor(_).get}
      // for each face, we build a set neighbourhood pairs
      val neighbors = (faces.distinct map { f => handle(f, faces diff Seq(f)) }).flatten.groupBy { e => e._1 }
      // we polish the neighbors pairs to store elements like [ref -> Set(neighbor1, neighbor2)]
      neighbors map { case (k,v) => { k -> v.map { _._2 }.toSet } }
    }
    // We merge the map obtained for each triangle into a global neighborhood one, and build a new FaceRegistry
    val reg = (mesh.faces /: merge(raw)) { (acc, pair) =>
      val f = acc(pair._1).copy(neighbors = Some(pair._2))
      acc.update(pair._1,f)
    }
    mesh.copy(faces = reg)
  }
}
