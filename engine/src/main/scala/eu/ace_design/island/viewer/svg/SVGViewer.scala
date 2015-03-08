package eu.ace_design.island.viewer.svg

import java.awt.Graphics2D
import java.awt.geom.Path2D
import java.io.File

import eu.ace_design.island.map._
import eu.ace_design.island.util.LogSilos
import eu.ace_design.island.viewer.Viewer


trait SVGEnhancer {
  val silo = LogSilos.VIEWER
  def apply(g: Graphics2D)
}

/**
 * The SVG viewer relies on the Apache Batik library (SVG) and the JTS library (to compute the convex hull of a face)
 */
trait SVGViewer extends Viewer  {
  import java.awt.geom.Line2D
  import java.awt.{BasicStroke, Color, Dimension, Graphics2D}

  import org.apache.batik.dom.svg.SVGDOMImplementation
  import org.apache.batik.svggen.SVGGraphics2D

  override val extension = "svg"
  override val mimeType = "image/svg+xml"

  protected val enhancers: Seq[SVGEnhancer] = Seq()

  /**
   * The function that actually draw the map, to be implemented by a concrete SVG viewer
   * @param m the map to draw
   * @param g the 2D canvas to be used
   */
  protected def draw(m: IslandMap, g: Graphics2D)

  /**
   * Syntactic sugar to call the viewer as a function
   * @param m the map one wants to visualize
   * @return a File containing the associated representation
   */
  override def apply(m: IslandMap): File = {
    info("Building an SVG document")
    val paintbrush = initGraphics(m)
    draw(m, paintbrush)
    enhancers foreach { _.apply(paintbrush) }
    val f = flush(paintbrush)
    debug(s"Storing the document in [${f.toString}]")
    f
  }

  /**
   * Transform a face into a Path2D that follows the convex hull fo the face.
   * @param faceRef the face reference
   * @param map the island map (used to compute the convex hull)
   * @return a turn-key Path2D (precision: Double)
   */
  protected def buildPath(faceRef: Int, map: IslandMap): Path2D = {
    val f = map.face(faceRef)
    // Compute the convex hull of this face to be sure that the drawn polygon is OK for the map
    val convexHull = map.convexHull(f)
    // Create a path for the Polygon frontier, and fill it according to the computed convex hull
    val path = new Path2D.Double()
    path.moveTo(convexHull(0).x,convexHull(0).y)
    convexHull.slice(1,convexHull.length) foreach { c => path.lineTo(c.x, c.y) }
    path.closePath()
    path
  }


  /**
   * Compute the gradient between two colors, according to a factor in [0,1]
   * @param c1 the first color  (associated to factor = 1)
   * @param c2 the second color (associated to factor = 0)
   * @param factor the factor (in [0,1]) to apply to compute the color to return
   * @return A color ro be used, corresponding to the gradient color between c1 and c2 for factor.
   */
  protected def gradient(c1: Color, c2: Color, factor: Double): Color = {
    require(factor >= 0.0 && factor <= 1.0, "factor must be in [0,1]")
    val comp1 = c1.getRGBComponents(null)
    val comp2 = c2.getRGBComponents(null)
    def get(i: Int): Float = (comp1(i)*factor + comp2(i)*(1-factor)).toFloat
    new Color(get(0), get(1), get(2))
  }

  /**
   * Initialise the graphics2D object used to draw the mesh.
   * @return
   */
  private def initGraphics(m: IslandMap): SVGGraphics2D = {
    val domImpl = SVGDOMImplementation.getDOMImplementation
    val document = domImpl.createDocument(SVGDOMImplementation.SVG_NAMESPACE_URI, "svg", null)
    val r = new SVGGraphics2D(document)
    r.setSVGCanvasSize(new Dimension(m.size, m.size))
    r
  }

  /**
   * Flush the mesh being drawn in an Graphics2D object into a plain file
   * @param svg2D the graphic object used to draw the mesh
   * @return the file used to flush the graphic object
   */
  private def flush(svg2D: SVGGraphics2D): File = {
    val result = initOutput
    svg2D.stream(result.getAbsolutePath, true) // true means "use CSS".
    result
  }

  /**
   * Highlight a given vertex to show its position on the map (useful for debugging wide maps)
   * @param vRef the vertex reference
   * @param m the map
   * @param g the graphics2D to be used to paint the vertex
   */
  protected def highlightVertex(vRef: Int, m: IslandMap, g: Graphics2D ): Unit = {
    g.setStroke(new BasicStroke(3f))
    val p = m.vertex(vRef)
    g.setColor(Color.RED)
    g.draw(new Line2D.Double(p.x, p.y, p.x, p.y))
  }

}

