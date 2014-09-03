package eu.ace_design.island.viewer.svg

import java.awt.geom.Line2D
import java.awt.{BasicStroke, Color, Graphics2D}
import eu.ace_design.island.map.{IsWater, IslandMap, Property}


/**
 * An heat-map displays the map as polygons, and map a color to each face, based on a given property. Faces tagged
 * as water has a blue dot in their center.
 *
 * The drawn color is defined as a gradient between a color (e.g., blue for moisture, red for temperature) and white.
 * The gradient factor is computed as 1 for the highest value, and 0 fot the lowest one. Faces that are not annotated
 * with the property are painted in black (can be configured).
 *
 * @param prop the property to map.
 * @param c the color to use as a start, going from this color for high values of prop to white for low values.
 * @param centers the color to be used to differentiate water faces (default to black)
 */
case class HeatMap(prop: Property[Double], c: Color = Color.RED, centers: Color = Color.BLACK) extends SVGViewer {


  protected def draw(m: IslandMap, g: Graphics2D): Unit = {

    val propValues = m.vertexProps.restrictedTo(prop)
    def factor(v: Double): Double = v / propValues.values.max

    // drawing each faces
    m.faceRefs foreach { ref =>
      val path = buildPath(ref, m)
      g.setStroke(new BasicStroke(1f))
      g.setColor(Color.BLACK); g.draw(path)

      val color = try {
        val value = propValues(m.face(ref).center)
        gradient(c, Color.WHITE, factor(value))
      } catch {
        case e: NoSuchElementException => Color.BLACK
      }
      g.setColor(color);       g.fill(path)

      if(m.faceProps.check(ref, IsWater())) {
        g.setColor(centers)
        g.setStroke(new BasicStroke(2f))
        val center = m.vertex(m.face(ref).center)
        g.draw(new Line2D.Double(center.x, center.y,center.x, center.y))
      }
    }
  }


}
