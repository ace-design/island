package eu.ace_design.island.viewer

import java.io.File
import eu.ace_design.island.map._

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

  object Colors {
    // classical colors
    final val BLACK      = new Color(0,   0,   0)
    final val WHITE      = new Color(255, 255, 255)
    final val LIGHT_GRAY = new Color(211, 211, 211)
    // Extracted from Cynthia Brewer palettes (http://colorbrewer2.org/)
    final val DARK_BLUE  = new Color(4  , 90 , 141) // 5-class PuBu theme  #5
    final val LIGHT_BLUE = new Color(116, 169, 207) // 5-class PuBu theme  #3
    final val LIGHT_SAND = new Color(255, 255, 204) // 9-class YlOrRd theme #1
  }

  /**
   * Syntactic sugar to call the viewer as a function
   * @param m the map one wants to visualize
   * @return a File containing the associated representation
   */
  override def apply(m: IslandMap): File = {
    info("Building an SVG document")
    val paintbrush = initGraphics(m)
    draw(m, paintbrush)
    val f = flush(paintbrush, m.mesh.size)
    debug(s"Storing the document in [${f.toString}]")
    f
  }

  /**
   * Actually draw a given mesh using a Graphics2D object (side-effect on g)
   * @param m the map one wants to draw
   * @param g the Graphics2D object used to draw the mesh
   */
  private def draw(m: IslandMap, g: Graphics2D) {
    // we rely on a set of function, executed sequentially to draw the map
    // Function must be member of Int x IslandMap x Graphics2D -> Unit
    val functions = Seq(
      drawAFace(_,_,_),
      drawNeighbors(_,_,_),
      drawCenters(_,_,_),
      drawCorners(_,_,_)
    )
    // We go through each function one by one. We apply each f to all the faces stored in the map
    functions foreach { f => m.mesh.faces.references foreach { f(_, m, g) } }
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
    debug(s"drawAFace(#$idx) using (bg=$bgColor, border=$border)")

    g.setStroke(new BasicStroke(0.5f))
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
    import ExistingWaterKind._

    val background = if(props.contains(WaterKind(OCEAN)))
      Colors.DARK_BLUE
    else if(props.contains(WaterKind(LAKE)))
      Colors.LIGHT_BLUE
    else if (props.contains(IsCoast()))
      Colors.LIGHT_SAND
    else
      Colors.WHITE

    val border = Colors.BLACK
    (background, border)
  }

  /**
   * Draw the center of each face as a single black point (width: 3). Mainly used for explanation purpose
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   */
  private def drawCenters(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.mesh.faces(idx)
    g.setColor(Colors.LIGHT_GRAY)
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
   * Draw the corners of each face, coloring water corners in blue and land one in black.
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   */
  private def drawCorners(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.mesh.faces(idx)
    g.setStroke(new BasicStroke(3))
    f.vertices(map.mesh.edges) foreach { ref =>
      if(map.vertexProps.check(ref, IsWater()))
        g.setColor(Colors.DARK_BLUE)
      else if (map.vertexProps.check(ref, IsCoast()))
        g.setColor(Colors.LIGHT_SAND)
      else
        g.setColor(Colors.BLACK)
      val p = map.mesh.vertices(ref)
      g.draw(new Line2D.Double(p.x, p.y,p.x, p.y))
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
