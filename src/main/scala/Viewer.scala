import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.Graphics2D
import scala.math.round

/**
 * Simple viewer to display the Island
 * source: http://www.cis.upenn.edu/~matuszek/Concise%20Guides/Concise%20Scala%20GUI.html
 * @author mosser
 */


object Viewer extends SimpleSwingApplication {

  final val SIZE = 200

  def top = new MainFrame { // top is a required method
    title = "Island's Map Viewer"
    val mesh = new SquaredMesh(SIZE, 15*15)
    val canvas = new MapCanvas(SIZE, mesh.faces)

    contents = new BorderPanel { layout(canvas) = Center }
    size = new Dimension(500, 500)

  }
}


class MapCanvas(val length: Int, val faces: Set[Face]) extends Panel {

  override val size = new Dimension(length, length)

  override def paintComponent(g: Graphics2D) {
    g.clearRect(0, 0, size.width, size.height)

    faces.foreach { face =>
      val xs = face.corners map { p => round(p.x).toInt }
      val ys = face.corners map { p => round(p.y).toInt }
      g.drawPolygon(xs.toArray, ys.toArray, face.corners.size)
    }

  }

}