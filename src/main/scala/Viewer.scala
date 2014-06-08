import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.Graphics2D

/**
 * Simple viewer to display the Island
 * source: http://www.cis.upenn.edu/~matuszek/Concise%20Guides/Concise%20Scala%20GUI.html
 * @author mosser
 */


object Viewer extends SimpleSwingApplication {

  def top = new MainFrame { // top is a required method
    title = "Island's Map Viewer"
    val canvas = new MapCanvas()
    contents = new BorderPanel { layout(canvas) = Center }
    size = new Dimension(500, 500)

  }
}


class MapCanvas extends Panel {

  preferredSize = new Dimension(500, 500)

  override def paintComponent(g: Graphics2D) {
    g.clearRect(0, 0, size.width, size.height)
    val xs: Array[Int] = Array(100, 100, 200, 200)
    val ys: Array[Int] = Array(100, 200, 200, 100)
    g.drawPolygon(xs, ys, 4)
  }

}