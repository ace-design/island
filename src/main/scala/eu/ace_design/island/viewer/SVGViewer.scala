package eu.ace_design.island.viewer

import java.awt.Color
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
  import ColorBrewer._

  override val extension = "svg"
  override val mimeType = "image/svg+xml"
  
  /**
   * Syntactic sugar to call the viewer as a function
   * @param m the map one wants to visualize
   * @return a File containing the associated representation
   */
  override def apply(m: IslandMap): File = {
    info("Building an SVG document")
    val paintbrush = initGraphics(m)
    draw(m, paintbrush)
    val f = flush(paintbrush)
    debug(s"Storing the document in [${f.toString}]")
    f
  }

  /**
   * Actually draw a given mesh using a Graphics2D object (side-effect on g)
   * @param m the map one wants to draw
   * @param g the Graphics2D object used to draw the mesh
   */
  private def draw(m: IslandMap, g: Graphics2D) {
    m.faceRefs foreach { drawABiome(_, m, g) }
    m.edgeRefs foreach { drawAnEdge(_, m, g) }
    //highlightVertex(1, m, g)
    if (m.uuid.isDefined) { g.setColor(BLACK); g.drawString(s"seed: ${m.uuid.get}", 5, m.size - 5) }
  }

  /**
   * Highlight a given vertex to show its position on the map (useful for debugging wide maps)
   * @param vRef the vertex reference
   * @param m the map
   * @param g the graphics2D to be used to paint the vertex
   */
  private def highlightVertex(vRef: Int, m: IslandMap, g: Graphics2D ): Unit = {
    g.setStroke(new BasicStroke(3f))
    val p = m.vertex(vRef)
    g.setColor(Color.RED)
    g.draw(new Line2D.Double(p.x, p.y, p.x, p.y))
  }


  /**
   * Draw a given face as a polygon on the map according to its biome. The used colors are extracted from the
   * biomePalette association function.
   *
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   */
  private def drawABiome(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.face(idx)

    // Compute the convex hull of this face to be sure that the drawn polygon is OK for the map
    val convexHull = map.convexHull(f)

    // Create a path for the Polygon frontier, and fill it according to the computed convex hull
    val path = new Path2D.Double()
    path.moveTo(convexHull(0).x,convexHull(0).y)
    convexHull.slice(1,convexHull.length) foreach { c => path.lineTo(c.x, c.y) }
    path.closePath()

    try {
      val biome = map.faceProps.getValue(idx, HasForBiome())
      g.setColor(biomePalette(biome))
    } catch { case e: IllegalArgumentException => g.setColor(WHITE) }
    g.setStroke(new BasicStroke(2f))
    g.draw(path)
    g.fill(path)
  }


  private def drawAnEdge(idx: Int, map: IslandMap, g: Graphics2D) {
    try {
      val flow = map.edgeProps.getValue(idx, RiverFlow()) // throw an exception if no river flows through this edge
      debug(s"edge #$idx with flow $flow")
      val edge = map.edge(idx)
      val p1 = map.vertex(edge.p1)
      val p2 = map.vertex(edge.p2)
      g.setStroke(new BasicStroke(2f * flow))
      g.setColor(MEDIUM_BLUE)
      g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y))
    } catch { case e: IllegalArgumentException => } // do nothing if not a river
  }

  /**
   *
   * @param biome
   * @return
   */
  private def biomePalette(biome: ExistingBiomes.Biome): Color = {
    import ExistingBiomes._
    biome match {
      /** Water faces **/
      case GLACIER => LIGHT_BLUE
      case LAKE    => MEDIUM_BLUE
      case OCEAN   => DARK_BLUE

      /** Dry biomes: beach, deserts and alpine rocks **/
      case BEACH               => LIGHT_YELLOW
      case SUB_TROPICAL_DESERT => MEDIUM_YELLOW
      case TEMPERATE_DESERT    => DARK_YELLOW
      case ALPINE              => DARK_GREY
        
      /** Dry biomes: grassland, shrubland and tundra **/
      case GRASSLAND => LIGHT_ORANGE
      case SHRUBLAND => MEDIUM_ORANGE
      case TUNDRA    => DARK_ORANGE

      /** Half-wet biomes: forests and taiga **/
      case TROPICAL_SEASONAL_FOREST   => ULTRA_LIGHT_GREEN
      case TEMPERATE_RAIN_FOREST      => LIGHT_GREEN
      case TROPICAL_RAIN_FOREST       => DARK_GREEN

      case TEMPERATE_DECIDUOUS_FOREST => MEDIUM_GREEN
      case TAIGA                      => ULTRA_DARK_GREEN

      /** Wet biomes: mangroves and snow  **/
      case MANGROVE => BROWN
      case SNOW     => WHITE

      /** Terra incognita **/
      case _ => BLACK
    }
  }


  private def gradient(c1: Color, c2: Color, value: Double): Color = {
    val comp1 = c1.getRGBComponents(null)
    val comp2 = c2.getRGBComponents(null)
    val factor = math.min(value / 100, 1)
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
   * Draw the center of each face as a single black point (width: 3). Mainly used for explanation purpose
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   * @deprecated (useful at the beginning of the project to display the underlying mesh)
   */
  private def drawCenters(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.face(idx)
    g.setColor(ColorBrewer.LIGHT_GREY)
    g.setStroke(new BasicStroke(2))
    val center = map.vertex(f.center)
    g.draw(new Line2D.Double(center.x, center.y,center.x, center.y))
  }

  /**
   * Draw the neighborhood relationship as gray lines between faces' centers. Mainly used for explanation purposes
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   * @deprecated (useful at the beginning of the project to display the underlying mesh)
   */
  private def drawNeighbors(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.face(idx)
    val center = map.vertex(f.center)
    g.setColor(Color.LIGHT_GRAY)
    g.setStroke(new BasicStroke(0.05f,BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, Array{4.0f}, 0.0f))
    f.neighbors match {
      case None =>
      case Some(refs) => refs foreach { idx =>
        val p = map.vertex(map.face(idx).center)
        g.draw(new Line2D.Double(center.x, center.y, p.x, p.y))
      }
    }
  }

  /**
   * Draw the corners of each face, coloring water corners in blue and land one in black.
   * @param idx the index of the face to draw
   * @param map the map used as a reference
   * @param g the graphics2D object used to paint
   * @deprecated (useful at the beginning of the project to display the underlying mesh)
   */
  private def drawCorners(idx: Int, map: IslandMap, g: Graphics2D) {
    val f = map.face(idx)
    g.setStroke(new BasicStroke(1))
    map.cornerRefs(f) foreach { ref =>
      if(map.vertexProps.check(ref, IsWater()))
        g.setColor(ColorBrewer.DARK_BLUE)
      else if (map.vertexProps.check(ref, IsCoast()))
        g.setColor(ColorBrewer.LIGHT_YELLOW)
      else
        g.setColor(ColorBrewer.BLACK)
      val p = map.vertex(ref)
      g.draw(new Line2D.Double(p.x, p.y,p.x, p.y))
    }
  }
}


/**
 * Color Palettes extracted from Cynthia Brewer colorbrewer awesome tool (http://colorbrewer2.org/)
 */
object ColorBrewer {
  // Classical colors (not from Brewer's palettes)
  final val BLACK = new Color(0,   0,   0)
  final val WHITE = new Color(255, 255, 255)
  final val BROWN = new Color(133, 127, 48)

  // Grays are defined with the 3-class Greys palette [http://colorbrewer2.org/?type=sequential&scheme=Greys&n=3]
  final val LIGHT_GREY  = new Color(240, 240, 240)
  final val MEDIUM_GREY = new Color(189, 189, 189)
  final val DARK_GREY   = new Color(99,  99,  99 )

  // Blues are defined with the 9-class PuBu palette [http://colorbrewer2.org/?type=sequential&scheme=PuBu&n=9]
  final val DARK_BLUE   = new Color(2,   56, 88 ) // 3-class PuBu theme  #9
  final val MEDIUM_BLUE = new Color(4,   90, 141) // 3-class PuBu theme  #8
  final val LIGHT_BLUE  = new Color(5,  112, 176) // 3-class PuBu theme  #7

  // Yellows are defined with the 6-class YlOrBr palette [http://colorbrewer2.org/?type=sequential&scheme=YlOrBr&n=6]
  final val LIGHT_YELLOW  = new Color(255, 255, 212) // 6-class YlOrBr theme #1
  final val MEDIUM_YELLOW = new Color(254, 227, 145) // 6-class YlOrBr theme #2
  final val DARK_YELLOW   = new Color(254, 198, 79 ) // 6-class YlOrBr theme #3

  // Oranges are defined with the 6-class YlOrBr palette [http://colorbrewer2.org/?type=sequential&scheme=YlOrBr&n=6]
  final val LIGHT_ORANGE  = new Color(254, 153, 41 ) // 6-class YlOrBr theme #4
  final val MEDIUM_ORANGE = new Color(217, 95 , 14 ) // 6-class YlOrBr theme #5
  final val DARK_ORANGE   = new Color(153, 52,  4  ) // 6-class YlOrBr theme #6

  // Greens are defined with the 9-class Greens palette [http://colorbrewer2.org/?type=sequential&scheme=Greens&n=9]
  final val ULTRA_DARK_GREEN  = new Color(0,   68,  27 ) // 9-class Greens #9
  final val DARK_GREEN        = new Color(0,   109, 44 ) // 9-class Greens #8
  final val MEDIUM_GREEN      = new Color(35,  139, 69 ) // 9-class Greens #7
  final val LIGHT_GREEN       = new Color(116, 198, 118) // 9-class Greens #5
  final val ULTRA_LIGHT_GREEN = new Color(161, 217, 165) // 9-class Greens #4

}

