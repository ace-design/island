package eu.ace_design.island.viewer.svg

import java.awt.geom.Line2D
import java.awt.{BasicStroke, Color, Graphics2D}
import eu.ace_design.island.map.{PropertySet, IsWater, IslandMap, Property}


/**
 * An heat-map displays the map as polygons, and map a color to each face, based on a given property. Faces tagged
 * as water has a blue dot in their center.
 *
 * The drawn color is defined as a gradient between a color (e.g., blue for moisture, red for temperature) and white.
 * The gradient factor is computed as 1 for the highest value, and 0 fot the lowest one. Faces that are not annotated
 * with the property are painted in black (can be configured).
 *
 * By default, the heat map generator uses the face property set, and uses each face reference to look for prop value.
 * One can specify a mapper to use another property set, and a selector to select the associated reference. This is for
 * example useful for the Elevation property, as it is defined on vertices instead of faces.
 *
 * @param prop the property to map.
 * @param c the color to use as a start, going from this color for high values of prop to white for low values.
 * @param centers the color to be used to differentiate water faces (default to black)
 * @param selector a function to select the property set that contains prop (default to face)
 * @param mapper a function to map a face to the index associated to prop in the selected property set (default to face
 *               reference)
 */
case class HeatMap(prop: Property[Double], c: Color = Color.RED, centers: Color = Color.BLACK,
                   selector: IslandMap => PropertySet = Selectors.faces,
                   mapper: (IslandMap, Int) => Int = Mappers.faceRef ) extends SVGViewer {



  protected def draw(m: IslandMap, g: Graphics2D): Unit = {

    val propValues = selector(m).restrictedTo(prop)
    def factor(v: Double): Double = v / propValues.values.max

    // drawing each faces
    m.faceRefs foreach { ref =>
      val path = buildPath(ref, m)
      g.setStroke(new BasicStroke(1f))
      g.setColor(Color.BLACK); g.draw(path)

      val color = try {
        val value = propValues(mapper(m, ref))
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

object Selectors {
  val vertices: IslandMap => PropertySet = m => m.vertexProps
  val faces: IslandMap => PropertySet = m => m.faceProps
}

object Mappers {
  val faceRef: (IslandMap, Int) => Int = (m,f) => f
  val faceCenterRef: (IslandMap, Int) => Int = (m,f) => m.face(f).center
}
