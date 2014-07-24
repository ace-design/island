package eu.ace_design.island.viewer


import eu.ace_design.island.map.{IsBorder, IslandMap, Property}
import java.io.File


/**
 * A viewer is an element used to represent a Mesh, processing it into a viewable file
 */
trait Viewer {
  /**
   * A viewer produces files with a given extension, usually compatible with external viewers.
   * @return the extension to be used
   */
  def extension: String

  /**
   * The MIME type used by this viewer (for the processed output file)
   * @return  a valid MIME type
   */
  def mimeType: String
  /**
   * A viewer is a function, transforming an IslandMap into a viewable file.
   * Thus, we use Scala syntactic sugar to support it
   *
   * @param m the map one wants to visualize
   * @return a File containing the associated representation
   */
  def apply(m: IslandMap): File

  /**
   * Protected method used to generate a temporary file as an output
   * @return
   */
  protected def initOutput: File = File.createTempFile("island-viewer-", "."+extension)

}


/**
 * The SVG viewer relies on the Apache Batik library (SVG) and the JTS library (to compute the convex hull of a face)
 */
class SVGViewer extends Viewer  {
  import java.awt.{Graphics2D,Color, Dimension,BasicStroke}
  import java.awt.geom.{Line2D, Path2D}
  import org.apache.batik.svggen.SVGGraphics2D
  import org.apache.batik.dom.svg.SVGDOMImplementation
  import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}

  override val extension = "svg"
  override val mimeType = "image/svg+xml"

  /**
   * Syntactic sugar to call the viewer as a function
   * @param m the map one wants to visualize
   * @return a File containing the associated representation
   */
  override def apply(m: IslandMap): File = {
    val paintbrush = initGraphics(m)
    draw(m, paintbrush)
    flush(paintbrush, m.mesh.size)
  }

  /**
   * Actually draw a given mesh using a Graphics2D object (side-effect on g)
   * @param m the map one wants to draw
   * @param g the Graphics2D object used to draw the mesh
   */
  private def draw(m: IslandMap, g: Graphics2D) {
    // Se rely on a set of function, executed sequentially to draw the map
    // Function must be member of Int x IslandMap x Graphics2D -> Unit
    val functions = Seq(drawAFace(_,_,_), drawNeighbors(_,_,_), drawCenters(_,_,_))
    // We go through each function one by one. We apply each f to all the faces stored in the map
    functions foreach { f => m.mesh.faces.references foreach { f(_, m, g) }
    }
  }

  /**
   * Draw a given face as a polygon on the map. The used colors are computed by the 'colors' function.
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   */
  private def drawAFace(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.mesh.faces(idx)

    // Compute the convex hull of this face to be sure that the drawn polygon is OK for the map
    val coords = (f.vertices(map.mesh.edges) map { map.mesh.vertices(_) } map { p => new Coordinate(p.x, p.y) }).toSeq
    val linear = coords :+ new Coordinate(coords(0).x, coords(0).y)
    val factory = new GeometryFactory()
    val convexCoords = factory.createPolygon(linear.toArray).convexHull.getCoordinates

    // Create a path for the Polygon frontier, and fill it according to the computed convex hull
    val path = new Path2D.Double()
    path.moveTo(convexCoords(0).x,convexCoords(0).y)
    convexCoords.slice(1,convexCoords.length) foreach { c => path.lineTo(c.x, c.y) }
    path.closePath()

    // Get the colors associated to this face, and draw it
    val (bgColor, border) = colors(map.faceProps.get(idx))
    g.setStroke(new BasicStroke(1))
    g.setColor(border)
    g.draw(path)
    g.setColor(bgColor)
    g.fill(path)
  }

  /**
   * Identify the colors to be used for a given polygon, based on its face properties
   * @param props the set of properties associated to this face
   * @return a couple (bgColor, borderColor) to be used to draw this face
   */
  private def colors(props: Set[Property[_]]): (Color, Color) = {
    val bg = if(props.contains(IsBorder())) Color.BLACK else Color.WHITE
    val border = Color.BLACK
    (bg, border)
  }

  /**
   * Draw the center of each face as a single black point (width: 3). Mainly used for explanation purpose
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   */
  private def drawCenters(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.mesh.faces(idx)
    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(3))
    val center = map.mesh.vertices(f.center)
    g.draw(new Line2D.Double(center.x, center.y,center.x, center.y))
  }

  /**
   * Draw the neighborhood relationship as gray lines between faces' centers. Mainly used for explanation purposes
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   */
  private def drawNeighbors(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.mesh.faces(idx)
    val center = map.mesh.vertices(f.center)
    g.setColor(Color.LIGHT_GRAY)
    g.setStroke(new BasicStroke(0.05f,BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, Array{4.0f}, 0.0f))
    f.neighbors match {
      case None =>
      case Some(refs) => refs foreach { idx =>
        val p = map.mesh.vertices(map.mesh.faces(idx).center)
        g.draw(new Line2D.Double(center.x, center.y, p.x, p.y))
      }
    }
  }

  /**
   * Initialise the graphics2D object used to draw the mesh.
   * @return
   */
  private def initGraphics(m: IslandMap): SVGGraphics2D = {
    val mesh = m.mesh
    val domImpl = SVGDOMImplementation.getDOMImplementation
    val document = domImpl.createDocument(SVGDOMImplementation.SVG_NAMESPACE_URI, "svg", null)
    val r = new SVGGraphics2D(document)
    mesh.size match {
      case Some(s) => r.setSVGCanvasSize(new Dimension(s, s))
      case None =>
    }
    r
  }

  /**
   * Flush the mesh being drawn in an Graphics2D object into a plain file
   * @param svg2D the graphic object used to draw the mesh
   * @param clip the size of the SVG file (attributes of the svg tag if present)
   * @return the file used to flush the graphic object
   */
  private def flush(svg2D: SVGGraphics2D, clip: Option[Int]): File = {
    val result = initOutput
    svg2D.stream(result.getAbsolutePath, true) // true means "use CSS".
    result
  }
}

class PDFViewer extends Viewer {
  import org.apache.batik.apps.rasterizer.{DestinationType, SVGConverter}

  override val extension: String = "pdf"
  override val mimeType: String = "application/pdf"

  override def apply(m: IslandMap): File = {
    val result = initOutput

    // We first create an SVG file with the SVG viewer:
    val svgFile = (new SVGViewer())(m)

    // We leverage the Batik rasterizer
    val converter = new SVGConverter()
    converter.setDestinationType(DestinationType.PDF)
    converter.setSources(Array(svgFile.getAbsolutePath))
    converter.setDst(result)

    // Running the converter and eventually returning the result
    // Batik has a f*cking System.out.println() instruction in its source => ugly patch
    System.setOut(new java.io.PrintStream(new java.io.ByteArrayOutputStream()))
    converter.execute()
    System.setOut(new java.io.PrintStream(new java.io.FileOutputStream(java.io.FileDescriptor.out)))
    result
  }

}