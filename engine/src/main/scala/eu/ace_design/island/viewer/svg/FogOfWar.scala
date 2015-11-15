package eu.ace_design.island.viewer.svg

import java.awt.geom.Ellipse2D
import java.awt.{BasicStroke, Color, Rectangle, Graphics2D}

class FogOfWar(factor: Int,
               visited: Set[(Int, Int)], scanned: Set[(Int,Int)],
               pois: Set[(Double, Double)], size: Int) extends SVGEnhancer {


  override def apply(g: Graphics2D): Unit = {

    // Setting up the color
    val gray = new Color(0,0,0,128)
    g.setColor(gray)
    g.setStroke(new BasicStroke(0.1f))

    // Drawing the grid
    (0 to size by factor) foreach { s =>
      g.drawLine(0,s,size,s)
      g.drawLine(s,0,s,size)
    }

    // Drawing POIs as red points
    g.setColor(Color.RED)
    pois.foreach { poi =>
      val circle = new Ellipse2D.Double(poi._1,poi._2, 3.0, 3.0)
      g.fill(circle)
    }

    val tiles = (for (x <- 0 to size / factor; y <- 0 to size / factor) yield (x,y)).toSet

    // Drawing dark rectangle on top of unvisited and unscanned tiles
    val incognita = tiles -- visited -- scanned
    g.setColor(new Color(0,0,0))
    incognita foreach { tile =>
      val rectangle = new Rectangle(tile._1*factor, tile._2*factor, factor, factor)
      g.fill(rectangle)
    }


    // Drawing transparent fog on top of unvisited tiles
    val fog = scanned -- visited
    g.setColor(gray)
    fog foreach { tile =>
      val rectangle = new Rectangle(tile._1*factor, tile._2*factor, factor, factor)
      g.fill(rectangle)
    }




  }
}
