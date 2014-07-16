package eu.ace_design.island.viewer

import java.io.File
import eu.ace_design.island.geom.Mesh


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
   * A viewer is a function, transforming a Mesh into a viewable file. Thus, we use Scala syntactic sugar to support it
   * @param mesh the mesh one wants to visualize
   * @return a File containing the associated representation
   */
  def apply(mesh: Mesh): File

}


/**
 * The SVG viewer relies on the Apache Batik library
 */
class SVGViewer extends Viewer  {
  import java.awt.{Graphics2D,Color, Dimension}
  import java.awt.geom.Path2D
  import org.apache.batik.svggen.SVGGraphics2D
  import org.apache.batik.dom.svg.SVGDOMImplementation

  override val extension = "svg"

  override val mimeType = "image/svg+xml"

  override def apply(mesh: Mesh): File = {
    val paintbrush = initGraphics(mesh)
    draw(mesh, paintbrush)
    flush(paintbrush, mesh.size)
  }

  /**
   * Actually draw a given mesh using a Graphics2D object (side-effect on g)
   * @param mesh the mesh to draw with g
   * @param g the Graphics2D object used to draw the mesh
   */
  private def draw(mesh: Mesh, g: Graphics2D) {
    g.setPaint(Color.red)
    mesh.faces.contents.keys foreach { f =>
      val path = new Path2D.Double()
      f.edges map { eRef => mesh.edges(eRef) } foreach { e =>
        val start = mesh.vertices(e.p1)
        val end   = mesh.vertices(e.p2)
        path.moveTo(start.x, start.y)
        path.lineTo(end.x, end.y)
      }
      g.draw(path)
    }
  }


  /**
   * Initialise the graphics2D object used to draw the mesh.
   * @return
   */
  private def initGraphics(mesh: Mesh): SVGGraphics2D = {
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
    val result = File.createTempFile("island-viewer-", "."+extension)
    svg2D.stream(result.getAbsolutePath, true) // true means "use CSS".
    result
  }
}
