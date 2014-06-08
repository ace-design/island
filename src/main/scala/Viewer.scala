import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.math.round

/**
 * Simple viewer to display the Island
 * source: http://www.cis.upenn.edu/~matuszek/Concise%20Guides/Concise%20Scala%20GUI.html
 * @author mosser
 */


object Viewer extends App {

  val mesh = new SquaredMesh(200, 15*15)
  val map = new ImageMap(mesh)
  map.toPNG("image.png")
}


class ImageMap(val mesh: Mesh) {


  private def build: BufferedImage = {
    val img = new BufferedImage(mesh.size, mesh.size, BufferedImage.TYPE_INT_BGR)
    val g = img.createGraphics()
    g.clearRect(0, 0, mesh.size, mesh.size)
    mesh.faces.foreach { face =>
      val xs = face.corners map { p => round(p.x).toInt }
      val ys = face.corners map { p => round(p.y).toInt }
      g.drawPolygon(xs.toArray, ys.toArray, face.corners.size)
    }
    img
  }

  def toPNG(fileName: String) = {
    val img = build
    val outputFile = new File(fileName)
    ImageIO.write(img, "png", outputFile)
  }

}